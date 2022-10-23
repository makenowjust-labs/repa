//! Regular expression syntax and its parser.
//!
//! # Syntax
//!
//! ```text
//! r ::= r r                                            # concat
//!     | r '|' r                                        # alternation
//!     | r '?' | r '*' | r '+'                          # repetition
//!     | r '{' n '}' | r '{' n ',}' | r '{' n ',' n '}'
//!     | '(' r ')'                                      # capture
//!     | '(?:' r ')'                                    # group
//!     | '.'                                            # dot
//!     | '[' k ']'                                      # character class
//!     | '{' x '}'                                      # variable increment
//!     | c                                              # literal
//!     | '^' | '$'                                      # zero-width assertion
//!     | '\b' | '\B' | '\A' | '\z'
//!     | '\d' | '\D' | '\s' | '\S' | '\w' | '\W'        # escape sequence character class
//!
//! k ::= k k | c | c '-' c
//!     | '\d' | '\D' | '\s' | '\S' | '\w' | '\W'
//!
//! n ::= [0-9]+
//! x ::= [a-zA-Z] [0-9a-zA-Z]*
//! c ::= 'a' | 'b' | ...
//!     | '\n' | '\r' | '\t'
//!     | '\|' | '\?' | '\*' | '\+' |
//!     | '\{' | '\}' | '\(' | '\)' | '\[' | '\]'
//!     | '\.' | '\^' | '\$' | '\\' | '\"' | '\''
//!     | '\u' [0-9a-fA-F]{4} | '\U' [0-9a-fA-F]{4}
//! ```
//!
//! Notes:
//!
//!   - A variables name cannot be reserved names in Presburger arithmetic formula (e.g. `for`, `all` and `true`)
//!   - A bounded quantifier must be ordered, so `a{2,1}` is invalid.
//!   - A character range in class also musb be ordered, so `[b-a]` is invalid.

use std::cell::Cell;

use once_cell::sync::Lazy;
use thiserror::Error;

use crate::presburger::Name;

/// A regular expression pattern.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Pattern {
    Alternation(Vec<Pattern>),
    Concat(Vec<Pattern>),
    Repetition(Box<Pattern>, RepetitionKind),
    Assertion(AssertionKind),
    Group(Box<Pattern>),
    Capture(usize, Box<Pattern>),
    Dot,
    Class(bool, Vec<ClassItem>),
    Literal(char),
    Increment(Name),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RepetitionKind {
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
    Exactly(usize),
    AtLeast(usize),
    Bounded(usize, usize),
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

static DIGIT: Lazy<Vec<ClassItem>> = Lazy::new(|| vec![ClassItem::Range('0', '9')]);

static WORD: Lazy<Vec<ClassItem>> = Lazy::new(|| {
    vec![
        ClassItem::Range('0', '9'),
        ClassItem::Range('A', 'Z'),
        ClassItem::Literal('_'),
        ClassItem::Range('a', 'z'),
    ]
});

static SPACE: Lazy<Vec<ClassItem>> = Lazy::new(|| {
    vec![
        ClassItem::Range('\t', '\u{b}'),
        ClassItem::Literal('\r'),
        ClassItem::Literal(' '),
    ]
});

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ParsingError {
    #[error("Character '{1}' is expected")]
    ExpectedChar(usize, char),
    #[error("End-of-string is expected")]
    ExpectedEndOfString(usize),
    #[error("Digit is expected")]
    ExpectedDigit(usize),
    #[error("Class item is invalid")]
    InvalidClassItem(usize),
    #[error("Digits are invalid")]
    InvalidDigit(usize),
    #[error("Escape sequence is invalid")]
    InvalidEscape(usize),
    #[error("Identifier is invalid")]
    InvalidIdent(usize),
    #[error("A lone metacharacter '{1}' is found")]
    LoneMetachar(usize, char),
    #[error("There is nothing to repeat")]
    NothingToRepeat(usize),
    #[error("Out-of-order class range")]
    OutOfOrderClassRange(usize),
    #[error("Out-of-order quantifier")]
    OutOfOrderQuantifier(usize),
    #[error("There is unclosed group")]
    UnclosedGroup(usize),
    #[error("End-of-string is unexpected")]
    UnexpectedEndOfString(usize),
    #[error("Escape sequence is unknown")]
    UnknownEscape(usize),
    #[error("Group kind specifier is unknown")]
    UnknownGroupKind(usize),
}

impl ParsingError {
    pub fn offset(&self) -> usize {
        match *self {
            ParsingError::ExpectedChar(offset, _) => offset,
            ParsingError::ExpectedEndOfString(offset) => offset,
            ParsingError::ExpectedDigit(offset) => offset,
            ParsingError::InvalidClassItem(offset) => offset,
            ParsingError::InvalidDigit(offset) => offset,
            ParsingError::InvalidEscape(offset) => offset,
            ParsingError::InvalidIdent(offset) => offset,
            ParsingError::LoneMetachar(offset, _) => offset,
            ParsingError::NothingToRepeat(offset) => offset,
            ParsingError::OutOfOrderClassRange(offset) => offset,
            ParsingError::OutOfOrderQuantifier(offset) => offset,
            ParsingError::UnclosedGroup(offset) => offset,
            ParsingError::UnexpectedEndOfString(offset) => offset,
            ParsingError::UnknownEscape(offset) => offset,
            ParsingError::UnknownGroupKind(offset) => offset,
        }
    }
}

#[derive(Debug)]
pub struct PatternParser<'s> {
    source: &'s str,
    offset_cell: Cell<usize>,
    capture_index_cell: Cell<usize>,
}

impl<'s> PatternParser<'s> {
    pub fn new(source: &'s str) -> PatternParser<'s> {
        PatternParser {
            source: source,
            offset_cell: Cell::new(0),
            capture_index_cell: Cell::new(1),
        }
    }

    pub fn parse(&self) -> Result<Pattern, ParsingError> {
        let child = self.parse_alternation()?;
        if !self.end_of_string() {
            return Err(ParsingError::ExpectedEndOfString(self.offset()));
        }
        Ok(child)
    }

    fn parse_alternation(&self) -> Result<Pattern, ParsingError> {
        let mut children = Vec::new();
        children.push(self.parse_concat()?);

        while !self.end_of_string() && self.current_char()? == '|' {
            self.next_char();
            children.push(self.parse_concat()?)
        }

        if children.len() == 1 {
            return Ok(children.first().unwrap().clone());
        }

        Ok(Pattern::Alternation(children))
    }

    fn parse_concat(&self) -> Result<Pattern, ParsingError> {
        let mut children = Vec::new();

        while !self.end_of_string() && self.current_char()? != '|' && self.current_char()? != ')' {
            children.push(self.parse_repetition()?)
        }

        if children.len() == 1 {
            return Ok(children.first().unwrap().clone());
        }

        Ok(Pattern::Concat(children))
    }

    fn parse_repetition(&self) -> Result<Pattern, ParsingError> {
        let child = self.parse_atom()?;

        if self.end_of_string() {
            return Ok(child);
        }

        match self.current_char()? {
            '?' => {
                self.next_char();
                Ok(Pattern::Repetition(
                    Box::new(child),
                    RepetitionKind::ZeroOrOne,
                ))
            }
            '*' => {
                self.next_char();
                Ok(Pattern::Repetition(
                    Box::new(child),
                    RepetitionKind::ZeroOrMore,
                ))
            }
            '+' => {
                self.next_char();
                Ok(Pattern::Repetition(
                    Box::new(child),
                    RepetitionKind::OneOrMore,
                ))
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
                        Ok(Pattern::Repetition(
                            Box::new(child),
                            RepetitionKind::Exactly(n),
                        ))
                    }
                    ',' => {
                        self.next_char();
                        if self.current_char()? == '}' {
                            self.next_char();
                            return Ok(Pattern::Repetition(
                                Box::new(child),
                                RepetitionKind::AtLeast(n),
                            ));
                        }
                        let m = self.parse_digits()?;
                        if self.current_char()? != '}' {
                            return Err(ParsingError::ExpectedChar(self.offset(), '}'));
                        }
                        self.next_char();
                        Ok(Pattern::Repetition(
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

    fn parse_digits(&self) -> Result<usize, ParsingError> {
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

    fn parse_atom(&self) -> Result<Pattern, ParsingError> {
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
                Ok(Pattern::Literal(value))
            }
        }
    }

    fn parse_increment(&self) -> Result<Pattern, ParsingError> {
        debug_assert!(self.current_char()? == '{');
        let start_offset = self.offset();
        self.next_char();
        if self.current_char()?.is_ascii_digit() {
            return Err(ParsingError::NothingToRepeat(start_offset));
        }
        let mut c = self.current_char()?;
        while c != '}' && (c.is_ascii_alphanumeric() || c == '_') {
            self.next_char();
            c = self.current_char()?;
        }
        self.next_char();
        let end_offset = self.offset();
        if start_offset + 1 == end_offset - 1 || c != '}' {
            return Err(ParsingError::InvalidIdent(start_offset));
        }
        let name = self.source[start_offset + 1..end_offset - 1].to_string();
        match name.as_str() {
            "for" | "all" | "some" | "and" | "or" | "not" | "true" | "false" => {
                Err(ParsingError::InvalidIdent(start_offset))
            }
            _ => Ok(Pattern::Increment(name.into())),
        }
    }

    fn parse_group(&self) -> Result<Pattern, ParsingError> {
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
                Ok(Pattern::Group(Box::new(child)))
            }
            _ => {
                let index = self.capture_index();
                self.next_capture_index();
                let child = self.parse_alternation()?;
                if self.current_char()? != ')' {
                    return Err(ParsingError::UnclosedGroup(self.offset()));
                }
                self.next_char();
                Ok(Pattern::Capture(index, Box::new(child)))
            }
        }
    }

    fn parse_class(&self) -> Result<Pattern, ParsingError> {
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

        Ok(Pattern::Class(not, items))
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
            Pattern::Literal(value) => Ok(ClassItem::Literal(value)),
            Pattern::Class(not, items) => Ok(ClassItem::Class(not, items)),
            _ => Err(ParsingError::InvalidClassItem(start_offset)),
        }
    }

    fn parse_dot(&self) -> Result<Pattern, ParsingError> {
        debug_assert!(self.current_char()? == '.');
        self.next_char();
        Ok(Pattern::Dot)
    }

    fn parse_start_line(&self) -> Result<Pattern, ParsingError> {
        debug_assert!(self.current_char()? == '^');
        self.next_char();
        Ok(Pattern::Assertion(AssertionKind::StartLine))
    }

    fn parse_end_line(&self) -> Result<Pattern, ParsingError> {
        debug_assert!(self.current_char()? == '$');
        self.next_char();
        Ok(Pattern::Assertion(AssertionKind::EndLine))
    }

    fn parse_escape(&self) -> Result<Pattern, ParsingError> {
        debug_assert!(self.current_char()? == '\\');
        let offset = self.offset();
        self.next_char();
        match self.current_char()? {
            'A' => {
                self.next_char();
                Ok(Pattern::Assertion(AssertionKind::StartText))
            }
            'B' => {
                self.next_char();
                Ok(Pattern::Assertion(AssertionKind::NotWordBoundary))
            }
            'D' => {
                self.next_char();
                Ok(Pattern::Class(true, DIGIT.clone()))
            }
            'S' => {
                self.next_char();
                Ok(Pattern::Class(true, SPACE.clone()))
            }
            'W' => {
                self.next_char();
                Ok(Pattern::Class(true, WORD.clone()))
            }
            'b' => {
                self.next_char();
                Ok(Pattern::Assertion(AssertionKind::WordBoundary))
            }
            'd' => {
                self.next_char();
                Ok(Pattern::Class(false, DIGIT.clone()))
            }
            'n' => {
                self.next_char();
                Ok(Pattern::Literal('\n'))
            }
            'r' => {
                self.next_char();
                Ok(Pattern::Literal('\r'))
            }
            's' => {
                self.next_char();
                Ok(Pattern::Class(false, SPACE.clone()))
            }
            't' => {
                self.next_char();
                Ok(Pattern::Literal('\t'))
            }
            'u' => {
                self.next_char();
                let value = self.parse_hexdigits(4)?;
                Ok(Pattern::Literal(value))
            }
            'U' => {
                self.next_char();
                let value = self.parse_hexdigits(8)?;
                Ok(Pattern::Literal(value))
            }
            'w' => {
                self.next_char();
                Ok(Pattern::Class(false, WORD.clone()))
            }
            'z' => {
                self.next_char();
                Ok(Pattern::Assertion(AssertionKind::EndText))
            }
            '|' | '?' | '*' | '+' | '{' | '}' | '.' | '^' | '$' | '(' | ')' | '[' | ']' | '\\'
            | '"' | '\'' => {
                let value = self.current_char()?;
                self.next_char();
                Ok(Pattern::Literal(value))
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
            .unwrap_or(Err(ParsingError::UnexpectedEndOfString(self.offset())))
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

#[cfg(test)]
fn parse(s: &str) -> Result<Pattern, ParsingError> {
    PatternParser::new(s).parse()
}

#[test]
fn test_parse_alternation() {
    assert_eq!(
        parse("a|b"),
        Ok(Pattern::Alternation(vec![
            Pattern::Literal('a'),
            Pattern::Literal('b')
        ]))
    );
}

#[test]
fn test_parse_concat() {
    assert_eq!(parse(""), Ok(Pattern::Concat(vec![])));
    assert_eq!(
        parse("abc"),
        Ok(Pattern::Concat(vec![
            Pattern::Literal('a'),
            Pattern::Literal('b'),
            Pattern::Literal('c')
        ]))
    );
}

#[test]
fn test_parse_repetition() {
    assert_eq!(
        parse(".?"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::ZeroOrOne
        ))
    );
    assert_eq!(
        parse(".*"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::ZeroOrMore
        ))
    );
    assert_eq!(
        parse(".+"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::OneOrMore
        ))
    );
    assert_eq!(
        parse(".{3}"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::Exactly(3)
        ))
    );
    assert_eq!(parse(".{3a"), Err(ParsingError::ExpectedChar(3, '}')));
    assert_eq!(
        parse(".{3,}"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::AtLeast(3)
        ))
    );
    assert_eq!(
        parse(".{3,5}"),
        Ok(Pattern::Repetition(
            Box::new(Pattern::Dot),
            RepetitionKind::Bounded(3, 5)
        ))
    );
}

#[test]
fn test_parse_atom() {
    assert_eq!(parse("?"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("*"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("+"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("{"), Err(ParsingError::UnexpectedEndOfString(1)));
    assert_eq!(parse("{1"), Err(ParsingError::NothingToRepeat(0)));
    assert_eq!(parse("{x}"), Ok(Pattern::Increment("x".into())));
    assert_eq!(
        parse("(?:)"),
        Ok(Pattern::Group(Box::new(Pattern::Concat(Vec::new()))))
    );
    assert_eq!(
        parse("()"),
        Ok(Pattern::Capture(1, Box::new(Pattern::Concat(Vec::new()))))
    );
    assert_eq!(parse("."), Ok(Pattern::Dot));
    assert_eq!(parse("^"), Ok(Pattern::Assertion(AssertionKind::StartLine)));
    assert_eq!(parse("$"), Ok(Pattern::Assertion(AssertionKind::EndLine)));
    assert_eq!(
        parse("\\A"),
        Ok(Pattern::Assertion(AssertionKind::StartText))
    );
    assert_eq!(
        parse("\\B"),
        Ok(Pattern::Assertion(AssertionKind::NotWordBoundary))
    );
    assert_eq!(parse("\\D"), Ok(Pattern::Class(true, DIGIT.clone())));
    assert_eq!(parse("\\S"), Ok(Pattern::Class(true, SPACE.clone())));
    assert_eq!(parse("\\W"), Ok(Pattern::Class(true, WORD.clone())));
    assert_eq!(
        parse("\\b"),
        Ok(Pattern::Assertion(AssertionKind::WordBoundary))
    );
    assert_eq!(parse("\\d"), Ok(Pattern::Class(false, DIGIT.clone())));
    assert_eq!(parse("\\n"), Ok(Pattern::Literal('\n')));
    assert_eq!(parse("\\r"), Ok(Pattern::Literal('\r')));
    assert_eq!(parse("\\s"), Ok(Pattern::Class(false, SPACE.clone())));
    assert_eq!(parse("\\t"), Ok(Pattern::Literal('\t')));
    assert_eq!(parse("\\u000b"), Ok(Pattern::Literal('\u{b}')));
    assert_eq!(parse("\\U0000000b"), Ok(Pattern::Literal('\u{b}')));
    assert_eq!(parse("\\w"), Ok(Pattern::Class(false, WORD.clone())));
    assert_eq!(parse("\\z"), Ok(Pattern::Assertion(AssertionKind::EndText)));
    assert_eq!(parse("\\|"), Ok(Pattern::Literal('|')));
    assert_eq!(parse("\\?"), Ok(Pattern::Literal('?')));
    assert_eq!(parse("\\*"), Ok(Pattern::Literal('*')));
    assert_eq!(parse("\\+"), Ok(Pattern::Literal('+')));
    assert_eq!(parse("\\{"), Ok(Pattern::Literal('{')));
    assert_eq!(parse("\\}"), Ok(Pattern::Literal('}')));
    assert_eq!(parse("\\."), Ok(Pattern::Literal('.')));
    assert_eq!(parse("\\^"), Ok(Pattern::Literal('^')));
    assert_eq!(parse("\\$"), Ok(Pattern::Literal('$')));
    assert_eq!(parse("\\("), Ok(Pattern::Literal('(')));
    assert_eq!(parse("\\)"), Ok(Pattern::Literal(')')));
    assert_eq!(parse("\\["), Ok(Pattern::Literal('[')));
    assert_eq!(parse("\\]"), Ok(Pattern::Literal(']')));
    assert_eq!(parse("\\\\"), Ok(Pattern::Literal('\\')));
    assert_eq!(parse("\\\""), Ok(Pattern::Literal('"')));
    assert_eq!(parse("\\'"), Ok(Pattern::Literal('\'')));
    assert_eq!(parse("\\]"), Ok(Pattern::Literal(']')));
    assert_eq!(parse("\\"), Err(ParsingError::UnexpectedEndOfString(1)));
    assert_eq!(parse("\\j"), Err(ParsingError::UnknownEscape(0)));
    assert_eq!(parse("a"), Ok(Pattern::Literal('a')));
    assert_eq!(parse("あ"), Ok(Pattern::Literal('あ')));
}

#[test]
fn test_parse_class() {
    assert_eq!(
        parse("[a]"),
        Ok(Pattern::Class(false, vec![ClassItem::Literal('a')]))
    );
    assert_eq!(
        parse("[^a]"),
        Ok(Pattern::Class(true, vec![ClassItem::Literal('a')]))
    );
    assert_eq!(parse("[]"), Ok(Pattern::Class(false, vec![])));
    assert_eq!(
        parse("[a-z]"),
        Ok(Pattern::Class(false, vec![ClassItem::Range('a', 'z')]))
    );
    assert_eq!(
        parse("[a-]"),
        Ok(Pattern::Class(
            false,
            vec![ClassItem::Literal('a'), ClassItem::Literal('-')]
        ))
    );
    assert_eq!(
        parse("[-z]"),
        Ok(Pattern::Class(
            false,
            vec![ClassItem::Literal('-'), ClassItem::Literal('z')]
        ))
    );
    assert_eq!(
        parse("[abc]"),
        Ok(Pattern::Class(
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
        Ok(Pattern::Class(
            false,
            vec![ClassItem::Class(false, WORD.clone())]
        ))
    );
}
