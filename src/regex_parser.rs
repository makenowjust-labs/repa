use std::cell::Cell;

use once_cell::sync::Lazy;
use thiserror::Error;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tree {
    Alternation(Vec<Tree>),
    Concat(Vec<Tree>),
    Repetition(Box<Tree>, RepetitionKind),
    Assertion(AssertionKind),
    Group(Box<Tree>),
    Capture(usize, Box<Tree>),
    Dot,
    Class(bool, Vec<ClassItem>),
    Literal(char),
    Increment(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Exactly(u32),
    AtLeast(u32),
    Bounded(u32, u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AssertionKind {
    StartLine,
    EndLine,
    StartText,
    EndText,
    WordBoundary,
    NotWordBoundary,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClassItem {
    Literal(char),
    Range(char, char),
    Class(bool, Vec<ClassItem>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Parser<'s> {
    source: &'s str,
    offset_cell: Cell<usize>,
    capture_index_cell: Cell<usize>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Parser<'s> {
        Parser {
            source: source,
            offset_cell: Cell::new(0),
            capture_index_cell: Cell::new(1),
        }
    }

    pub fn parse(&self) -> Result<Tree, ParsingError> {
        let child = self.parse_alternation()?;
        if !self.end_of_string() {
            return Err(ParsingError::ExpectedEof(self.offset()));
        }
        Ok(child)
    }

    fn parse_alternation(&self) -> Result<Tree, ParsingError> {
        let mut children = Vec::new();
        children.push(self.parse_concat()?);

        while !self.end_of_string() && self.current_char()? == '|' {
            self.next_char();
            children.push(self.parse_concat()?)
        }

        if children.len() == 1 {
            return Ok(children.first().unwrap().clone());
        }

        Ok(Tree::Alternation(children))
    }

    fn parse_concat(&self) -> Result<Tree, ParsingError> {
        let mut children = Vec::new();

        while !self.end_of_string() && self.current_char()? != '|' && self.current_char()? != ')' {
            children.push(self.parse_repetition()?)
        }

        if children.len() == 1 {
            return Ok(children.first().unwrap().clone());
        }

        Ok(Tree::Concat(children))
    }

    fn parse_repetition(&self) -> Result<Tree, ParsingError> {
        let child = self.parse_atom()?;

        if self.end_of_string() {
            return Ok(child);
        }

        match self.current_char()? {
            '?' => {
                self.next_char();
                Ok(Tree::Repetition(Box::new(child), RepetitionKind::ZeroOrOne))
            }
            '*' => {
                self.next_char();
                Ok(Tree::Repetition(
                    Box::new(child),
                    RepetitionKind::ZeroOrMore,
                ))
            }
            '+' => {
                self.next_char();
                Ok(Tree::Repetition(Box::new(child), RepetitionKind::OneOrMore))
            }
            '{' => {
                let saved_offset = self.offset();
                self.next_char();
                if !self.current_char()?.is_ascii_digit() {
                    self.reset_offset(saved_offset);
                    return Ok(child);
                }
                let n = self.parse_digits()?;
                match self.current_char()? {
                    '}' => {
                        self.next_char();
                        Ok(Tree::Repetition(
                            Box::new(child),
                            RepetitionKind::Exactly(n),
                        ))
                    }
                    ',' => {
                        self.next_char();
                        if self.current_char()? == '}' {
                            self.next_char();
                            return Ok(Tree::Repetition(
                                Box::new(child),
                                RepetitionKind::AtLeast(n),
                            ));
                        }
                        let m = self.parse_digits()?;
                        if self.current_char()? != '}' {
                            return Err(ParsingError::ExpectedChar(self.offset(), '}'));
                        }
                        self.next_char();
                        Ok(Tree::Repetition(
                            Box::new(child),
                            RepetitionKind::Bounded(n, m),
                        ))
                    }
                    _ => Err(ParsingError::ExpectedChar(self.offset(), '}')),
                }
            }
            _ => Ok(child),
        }
    }

    fn parse_digits(&self) -> Result<u32, ParsingError> {
        debug_assert!(self.current_char()?.is_ascii_digit());

        let start_offset = self.offset();
        while self.current_char()?.is_ascii_digit() {
            self.next_char()
        }
        let end_offset = self.offset();

        let n = match self.source[start_offset..end_offset].parse() {
            Ok(n) => n,
            Err(_) => return Err(ParsingError::InvalidDigit(start_offset)),
        };
        Ok(n)
    }

    fn parse_atom(&self) -> Result<Tree, ParsingError> {
        match self.current_char()? {
            '?' => Err(ParsingError::NothingToRepeat(self.offset())),
            '*' => Err(ParsingError::NothingToRepeat(self.offset())),
            '+' => Err(ParsingError::NothingToRepeat(self.offset())),
            '{' => self.parse_increment(),
            '}' => Err(ParsingError::LoneMetachar(self.offset(), '}')),
            '(' => self.parse_group(),
            ')' => Err(ParsingError::LoneMetachar(self.offset(), ')')),
            '[' => self.parse_class(),
            ']' => Err(ParsingError::LoneMetachar(self.offset(), ']')),
            '.' => self.parse_dot(),
            '^' => self.parse_start_line(),
            '$' => self.parse_end_line(),
            '\\' => self.parse_escape(),
            value => {
                self.next_char();
                Ok(Tree::Literal(value))
            }
        }
    }

    fn parse_increment(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '{');
        let start_offset = self.offset();
        self.next_char();
        if self.current_char()?.is_ascii_digit() {
            return Err(ParsingError::NothingToRepeat(start_offset));
        }
        while self.current_char()? != '}' {
            self.next_char();
        }
        self.next_char();
        let end_offset = self.offset();
        let name = self.source[start_offset + 1..end_offset - 1].to_string();
        Ok(Tree::Increment(name))
    }

    fn parse_group(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '(');
        self.next_char();
        match self.current_char()? {
            '?' => {
                self.next_char();
                if self.current_char()? != ':' {
                    return Err(ParsingError::UnknownGroupKind(self.offset()));
                }
                self.next_char();
                let child = self.parse_alternation()?;
                if self.current_char()? != ')' {
                    return Err(ParsingError::UnclosedGroup(self.offset()));
                }
                self.next_char();
                Ok(Tree::Group(Box::new(child)))
            }
            _ => {
                let index = self.capture_index();
                self.next_capture_index();
                let child = self.parse_alternation()?;
                if self.current_char()? != ')' {
                    return Err(ParsingError::UnclosedGroup(self.offset()));
                }
                self.next_char();
                Ok(Tree::Capture(index, Box::new(child)))
            }
        }
    }

    fn parse_class(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '[');
        self.next_char();

        let not = self.current_char()? == '^';
        if not {
            self.next_char();
        }

        let mut items = Vec::new();
        while self.current_char()? != ']' {
            items.push(self.parse_class_item()?);
        }

        debug_assert!(self.current_char()? == ']');
        self.next_char();

        Ok(Tree::Class(not, items))
    }

    fn parse_class_item(&self) -> Result<ClassItem, ParsingError> {
        let start_offset = self.offset();

        let left = match self.current_char()? {
            '\\' => self.parse_class_escape()?,
            value => {
                self.next_char();
                ClassItem::Literal(value)
            }
        };

        if self.current_char()? != '-' {
            return Ok(left);
        }
        let minus_offset = self.offset();
        self.next_char();

        let right = match self.current_char()? {
            '\\' => self.parse_class_escape()?,
            ']' => {
                self.reset_offset(minus_offset);
                return Ok(left);
            }
            value => {
                self.next_char();
                ClassItem::Literal(value)
            }
        };

        match (left, right) {
            (ClassItem::Literal(l), ClassItem::Literal(r)) => {
                if l > r {
                    return Err(ParsingError::OutOfOrderClassRange(start_offset));
                }
                Ok(ClassItem::Range(l, r))
            }
            _ => Err(ParsingError::InvalidClassItem(start_offset)),
        }
    }

    fn parse_class_escape(&self) -> Result<ClassItem, ParsingError> {
        let start_offset = self.offset();
        match self.parse_escape()? {
            Tree::Literal(value) => Ok(ClassItem::Literal(value)),
            Tree::Class(not, items) => Ok(ClassItem::Class(not, items)),
            _ => Err(ParsingError::InvalidClassItem(start_offset)),
        }
    }

    fn parse_dot(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '.');
        self.next_char();
        Ok(Tree::Dot)
    }

    fn parse_start_line(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '^');
        self.next_char();
        Ok(Tree::Assertion(AssertionKind::StartLine))
    }

    fn parse_end_line(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '$');
        self.next_char();
        Ok(Tree::Assertion(AssertionKind::EndLine))
    }

    fn parse_escape(&self) -> Result<Tree, ParsingError> {
        debug_assert!(self.current_char()? == '\\');
        let offset = self.offset();
        self.next_char();
        match self.current_char()? {
            'A' => {
                self.next_char();
                Ok(Tree::Assertion(AssertionKind::StartText))
            }
            'B' => {
                self.next_char();
                Ok(Tree::Assertion(AssertionKind::NotWordBoundary))
            }
            'D' => {
                self.next_char();
                Ok(Tree::Class(true, DIGIT.clone()))
            }
            'S' => {
                self.next_char();
                Ok(Tree::Class(true, SPACE.clone()))
            }
            'W' => {
                self.next_char();
                Ok(Tree::Class(true, WORD.clone()))
            }
            'b' => {
                self.next_char();
                Ok(Tree::Assertion(AssertionKind::WordBoundary))
            }
            'd' => {
                self.next_char();
                Ok(Tree::Class(false, DIGIT.clone()))
            }
            'n' => {
                self.next_char();
                Ok(Tree::Literal('\n'))
            }
            'r' => {
                self.next_char();
                Ok(Tree::Literal('\r'))
            }
            's' => {
                self.next_char();
                Ok(Tree::Class(false, SPACE.clone()))
            }
            't' => {
                self.next_char();
                Ok(Tree::Literal('\t'))
            }
            'u' => {
                self.next_char();
                let value = self.parse_hexdigits(4)?;
                Ok(Tree::Literal(value))
            }
            'U' => {
                self.next_char();
                let value = self.parse_hexdigits(8)?;
                Ok(Tree::Literal(value))
            }
            'w' => {
                self.next_char();
                Ok(Tree::Class(false, WORD.clone()))
            }
            'z' => {
                self.next_char();
                Ok(Tree::Assertion(AssertionKind::EndText))
            }
            '|' | '?' | '*' | '+' | '{' | '}' | '.' | '^' | '$' | '(' | ')' | '[' | ']' | '\\'
            | '"' | '\'' => {
                let value = self.current_char()?;
                self.next_char();
                Ok(Tree::Literal(value))
            }
            _ => Err(ParsingError::UnknownEscape(offset)),
        }
    }

    fn parse_hexdigits(&self, n: usize) -> Result<char, ParsingError> {
        let start_offset = self.offset();
        let mut i = 0;
        while i < n {
            if !self.current_char()?.is_ascii_hexdigit() {
                return Err(ParsingError::InvalidEscape(start_offset - 2));
            }
            self.next_char();
            i += 1;
        }

        let n = u32::from_str_radix(&self.source[start_offset..self.offset()], 16);
        match n.map(char::from_u32) {
            Ok(Some(value)) => Ok(value),
            _ => Err(ParsingError::InvalidDigit(start_offset)),
        }
    }

    fn current_char(&self) -> Result<char, ParsingError> {
        self.source[self.offset()..]
            .chars()
            .next()
            .map(|x| Ok(x))
            .unwrap_or(Err(ParsingError::UnexpectedEof(self.offset())))
    }

    fn next_char(&self) {
        self.offset_cell
            .set(self.offset() + self.current_char().map_or(0, |c| c.len_utf8()));
    }

    fn end_of_string(&self) -> bool {
        self.offset() == self.source.len()
    }

    fn offset(&self) -> usize {
        self.offset_cell.get()
    }

    fn reset_offset(&self, offset: usize) {
        self.offset_cell.set(offset);
    }

    fn capture_index(&self) -> usize {
        self.capture_index_cell.get()
    }

    fn next_capture_index(&self) {
        self.capture_index_cell.set(self.capture_index() + 1)
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ParsingError {
    #[error("'{1}' is expected")]
    ExpectedChar(usize, char),
    #[error("end-of-string is expected")]
    ExpectedEof(usize),
    #[error("digit is expected")]
    ExpectedDigit(usize),
    #[error("class item is invalid")]
    InvalidClassItem(usize),
    #[error("digits are invalid")]
    InvalidDigit(usize),
    #[error("escape sequence is invalid")]
    InvalidEscape(usize),
    #[error("a lone metacharacter '{1}' is found")]
    LoneMetachar(usize, char),
    #[error("there is nothing to repeat")]
    NothingToRepeat(usize),
    #[error("out-of-order class range")]
    OutOfOrderClassRange(usize),
    #[error("out-of-order quantifier")]
    OutOfOrderQuantifier(usize),
    #[error("there is unclosed group")]
    UnclosedGroup(usize),
    #[error("end-of-string is unexpected")]
    UnexpectedEof(usize),
    #[error("escape sequence is unknown")]
    UnknownEscape(usize),
    #[error("group kind specifier is unknown")]
    UnknownGroupKind(usize),
}

impl ParsingError {
    pub fn offset(&self) -> usize {
        match *self {
            ParsingError::ExpectedChar(offset, _) => offset,
            ParsingError::ExpectedEof(offset) => offset,
            ParsingError::ExpectedDigit(offset) => offset,
            ParsingError::InvalidClassItem(offset) => offset,
            ParsingError::InvalidDigit(offset) => offset,
            ParsingError::InvalidEscape(offset) => offset,
            ParsingError::LoneMetachar(offset, _) => offset,
            ParsingError::NothingToRepeat(offset) => offset,
            ParsingError::OutOfOrderClassRange(offset) => offset,
            ParsingError::OutOfOrderQuantifier(offset) => offset,
            ParsingError::UnclosedGroup(offset) => offset,
            ParsingError::UnexpectedEof(offset) => offset,
            ParsingError::UnknownEscape(offset) => offset,
            ParsingError::UnknownGroupKind(offset) => offset,
        }
    }
}

static DIGIT: Lazy<Vec<ClassItem>> = Lazy::new(|| {
    let mut v = Vec::new();
    v.push(ClassItem::Range('0', '9'));
    v
});

static WORD: Lazy<Vec<ClassItem>> = Lazy::new(|| {
    let mut v = Vec::new();
    v.push(ClassItem::Range('0', '9'));
    v.push(ClassItem::Range('A', 'Z'));
    v.push(ClassItem::Literal('_'));
    v.push(ClassItem::Range('a', 'z'));
    v
});

static SPACE: Lazy<Vec<ClassItem>> = Lazy::new(|| {
    let mut v = Vec::new();
    v.push(ClassItem::Range('\t', '\u{b}'));
    v.push(ClassItem::Literal('\r'));
    v.push(ClassItem::Literal(' '));
    v
});

#[cfg(test)]
fn parse<'s>(s: &'s str) -> Result<Tree, ParsingError> {
    Parser::new(s).parse()
}

#[test]
fn test_parse_alternation() {
    assert_eq!(
        parse("a|b"),
        Ok(Tree::Alternation(vec![
            Tree::Literal('a'),
            Tree::Literal('b')
        ]))
    );
}

#[test]
fn test_parse_concat() {
    assert_eq!(parse(""), Ok(Tree::Concat(vec![])));
    assert_eq!(
        parse("abc"),
        Ok(Tree::Concat(vec![
            Tree::Literal('a'),
            Tree::Literal('b'),
            Tree::Literal('c')
        ]))
    );
}

#[test]
fn test_parse_repetition() {
    assert_eq!(
        parse(".?"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::ZeroOrOne
        ))
    );
    assert_eq!(
        parse(".*"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::ZeroOrMore
        ))
    );
    assert_eq!(
        parse(".+"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::OneOrMore
        ))
    );
    assert_eq!(
        parse(".{3}"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::Exactly(3)
        ))
    );
    assert_eq!(parse(".{3a"), Err(ParsingError::ExpectedChar(3, '}')));
    assert_eq!(
        parse(".{3,}"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::AtLeast(3)
        ))
    );
    assert_eq!(
        parse(".{3,5}"),
        Ok(Tree::Repetition(
            Box::new(Tree::Dot),
            RepetitionKind::Bounded(3, 5)
        ))
    );
}

#[test]
fn test_parse_atom() {
    assert_eq!(parse("?"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("*"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("+"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("{"), Err(ParsingError::UnexpectedEof(1)));
    assert_eq!(parse("{1"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("{x}"), Ok(Tree::Increment("x".to_string())));
    assert_eq!(
        parse("(?:)"),
        Ok(Tree::Group(Box::new(Tree::Concat(Vec::new()))))
    );
    assert_eq!(
        parse("()"),
        Ok(Tree::Capture(1, Box::new(Tree::Concat(Vec::new()))))
    );
    assert_eq!(parse("."), Ok(Tree::Dot));
    assert_eq!(parse("^"), Ok(Tree::Assertion(AssertionKind::StartLine)));
    assert_eq!(parse("$"), Ok(Tree::Assertion(AssertionKind::EndLine)));
    assert_eq!(parse("\\A"), Ok(Tree::Assertion(AssertionKind::StartText)));
    assert_eq!(
        parse("\\B"),
        Ok(Tree::Assertion(AssertionKind::NotWordBoundary))
    );
    assert_eq!(parse("\\D"), Ok(Tree::Class(true, DIGIT.clone())));
    assert_eq!(parse("\\S"), Ok(Tree::Class(true, SPACE.clone())));
    assert_eq!(parse("\\W"), Ok(Tree::Class(true, WORD.clone())));
    assert_eq!(
        parse("\\b"),
        Ok(Tree::Assertion(AssertionKind::WordBoundary))
    );
    assert_eq!(parse("\\d"), Ok(Tree::Class(false, DIGIT.clone())));
    assert_eq!(parse("\\n"), Ok(Tree::Literal('\n')));
    assert_eq!(parse("\\r"), Ok(Tree::Literal('\r')));
    assert_eq!(parse("\\s"), Ok(Tree::Class(false, SPACE.clone())));
    assert_eq!(parse("\\t"), Ok(Tree::Literal('\t')));
    assert_eq!(parse("\\u000b"), Ok(Tree::Literal('\u{b}')));
    assert_eq!(parse("\\U0000000b"), Ok(Tree::Literal('\u{b}')));
    assert_eq!(parse("\\w"), Ok(Tree::Class(false, WORD.clone())));
    assert_eq!(parse("\\z"), Ok(Tree::Assertion(AssertionKind::EndText)));
    assert_eq!(parse("\\|"), Ok(Tree::Literal('|')));
    assert_eq!(parse("\\?"), Ok(Tree::Literal('?')));
    assert_eq!(parse("\\*"), Ok(Tree::Literal('*')));
    assert_eq!(parse("\\+"), Ok(Tree::Literal('+')));
    assert_eq!(parse("\\{"), Ok(Tree::Literal('{')));
    assert_eq!(parse("\\}"), Ok(Tree::Literal('}')));
    assert_eq!(parse("\\."), Ok(Tree::Literal('.')));
    assert_eq!(parse("\\^"), Ok(Tree::Literal('^')));
    assert_eq!(parse("\\$"), Ok(Tree::Literal('$')));
    assert_eq!(parse("\\("), Ok(Tree::Literal('(')));
    assert_eq!(parse("\\)"), Ok(Tree::Literal(')')));
    assert_eq!(parse("\\["), Ok(Tree::Literal('[')));
    assert_eq!(parse("\\]"), Ok(Tree::Literal(']')));
    assert_eq!(parse("\\\\"), Ok(Tree::Literal('\\')));
    assert_eq!(parse("\\\""), Ok(Tree::Literal('"')));
    assert_eq!(parse("\\'"), Ok(Tree::Literal('\'')));
    assert_eq!(parse("\\]"), Ok(Tree::Literal(']')));
    assert_eq!(parse("\\"), Err(ParsingError::UnexpectedEof(1)));
    assert_eq!(parse("\\j"), Err(ParsingError::UnknownEscape(0)));
    assert_eq!(parse("a"), Ok(Tree::Literal('a')));
    assert_eq!(parse("あ"), Ok(Tree::Literal('あ')));
}

#[test]
fn test_parse_class() {
    assert_eq!(
        parse("[a]"),
        Ok(Tree::Class(false, vec![ClassItem::Literal('a')]))
    );
    assert_eq!(
        parse("[^a]"),
        Ok(Tree::Class(true, vec![ClassItem::Literal('a')]))
    );
    assert_eq!(parse("[]"), Ok(Tree::Class(false, vec![])));
    assert_eq!(
        parse("[a-z]"),
        Ok(Tree::Class(false, vec![ClassItem::Range('a', 'z')]))
    );
    assert_eq!(
        parse("[a-]"),
        Ok(Tree::Class(
            false,
            vec![ClassItem::Literal('a'), ClassItem::Literal('-')]
        ))
    );
    assert_eq!(
        parse("[-z]"),
        Ok(Tree::Class(
            false,
            vec![ClassItem::Literal('-'), ClassItem::Literal('z')]
        ))
    );
    assert_eq!(
        parse("[abc]"),
        Ok(Tree::Class(
            false,
            vec![
                ClassItem::Literal('a'),
                ClassItem::Literal('b'),
                ClassItem::Literal('c')
            ]
        ))
    );
    assert_eq!(
        parse("[\\w]"),
        Ok(Tree::Class(
            false,
            vec![ClassItem::Class(false, WORD.clone())]
        ))
    );
}
