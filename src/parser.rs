use std::cell::Cell;

use once_cell::sync::Lazy;

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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RepetitionKind {
  ZeroOrOne,
  ZeroOrMore,
  OneOrMore,
  Exactly(u32),
  AtLeast(u32),
  Bounded(u32, u32)
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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Parser<'s> {
  source: &'s str,
  offset_cell: Cell<usize>,
  capture_index_cell: Cell<usize>,
}

impl<'s> Parser<'s> {
  fn new(source: &'s str) -> Parser<'s> {
    Parser {
      source: source,
      offset_cell: Cell::new(0),
      capture_index_cell: Cell::new(1),
    }
  }

  pub fn parse(&self) -> Result<Tree, ParsingError> {
    let child = self.parse_alternation()?;
    if !self.end_of_string() {
      return Err(ParsingError::ExpectedEof(self.offset()))
    }
    Ok(child)
  }

  fn parse_alternation(&self) -> Result<Tree, ParsingError> {
    let mut children = Vec::new();
    children.push(self.parse_concat()?);

    while self.current_char() == '|' {
      children.push(self.parse_concat()?)
    }

    if children.len() == 1 {
      return Ok(children.first().unwrap().clone())
    }

    Ok(Tree::Alternation(children))
  }

  fn parse_concat(&self) -> Result<Tree, ParsingError> {
    let mut children = Vec::new();

    while self.current_char() != '|' && self.current_char() != ')' && !self.end_of_string() {
      children.push(self.parse_repetition()?)
    }

    if children.len() == 1 {
      return Ok(children.first().unwrap().clone())
    }

    Ok(Tree::Concat(children))
  }

  fn parse_repetition(&self) -> Result<Tree, ParsingError> {
    let child = self.parse_atom()?;

    match self.current_char() {
      '?' => {
        self.next_char();
        Ok(Tree::Repetition(Box::new(child), RepetitionKind::ZeroOrOne))
      },
      '*' => {
        self.next_char();
        Ok(Tree::Repetition(Box::new(child), RepetitionKind::ZeroOrMore))
      },
      '+' => {
        self.next_char();
        Ok(Tree::Repetition(Box::new(child), RepetitionKind::OneOrMore))
      },
      '{' => {
        self.next_char();
        let n = self.parse_digits()?;
        match self.current_char() {
          '}' => {
            self.next_char();
            Ok(Tree::Repetition(Box::new(child), RepetitionKind::Exactly(n)))          
          },
          ',' => {
            self.next_char();
            if self.current_char() == '}' {
              self.next_char();
              return Ok(Tree::Repetition(Box::new(child), RepetitionKind::AtLeast(n)))
            }
            let m = self.parse_digits()?;
            if self.current_char() != '}' {
              return Err(ParsingError::ExpectedChar(self.offset(), '}'))
            }
            self.next_char();
            Ok(Tree::Repetition(Box::new(child), RepetitionKind::Bounded(n, m)))
          },
          _ => Err(ParsingError::ExpectedChar(self.offset(), '}')),
        }
      },
      _ => Ok(child)
    }
  }

  fn parse_digits(&self) -> Result<u32, ParsingError> {
    let start_offset = self.offset();

    while '0' <= self.current_char() && self.current_char() <= '9' {
      self.next_char()
    }

    let end_offset = self.offset();
    if start_offset == end_offset {
      return Err(ParsingError::ExpectedDigit(start_offset))
    }

    let n = match self.source[start_offset..end_offset].parse() {
      Ok(n) => n,
      Err(_) => return Err(ParsingError::InvalidDigit(start_offset)),
    };

    Ok(n)
  }

  fn parse_atom(&self) -> Result<Tree, ParsingError> {
    match self.current_char() {
      '.' => {
        self.next_char();
        Ok(Tree::Dot)
      },
      '^' => {
        self.next_char();
        Ok(Tree::Assertion(AssertionKind::StartLine))
      },
      '$' => {
        self.next_char();
        Ok(Tree::Assertion(AssertionKind::EndLine))
      },
      '\\' => {
        self.next_char();
      },
      '?' => Err(ParsingError::NothingToRepeat(self.offset())),
      '*' => Err(ParsingError::NothingToRepeat(self.offset())),
      '+' => Err(ParsingError::NothingToRepeat(self.offset())),
      '{' => Err(ParsingError::NothingToRepeat(self.offset())),
      '}' => Err(ParsingError::LoneMetachar(self.offset(), '}')),
      ')' => Err(ParsingError::LoneMetachar(self.offset(), ')')),
      ']' => Err(ParsingError::LoneMetachar(self.offset(), ']')),
      value => {
        self.next_char();
        Ok(Tree::Literal(value))
      },
    }
  }

  fn current_char(&self) -> Result<char, ParsingError> {
    self.source[self.offset()..].chars().next().map(|x| Ok(x)).unwrap_or(|| Err(ParsingError::UnexpectedEof))
  }

  fn next_char(&self) {
    self.offset_cell.set(self.offset() + self.current_char().len_utf8())
  }

  fn offset(&self) -> usize {
    self.offset_cell.get()
  }

  fn end_of_string(&self) -> bool {
    self.offset() == self.source.len()
  }
}

pub enum ParsingError {
  ExpectedChar(usize, char),
  ExpectedEof(usize),
  ExpectedDigit(usize),
  UnexpectedEof,
  InvalidDigit(usize),
  NothingToRepeat(usize),
  LoneMetachar(usize, char),
  OutOfOrderQuantifier(usize),
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
  v.push(ClassItem::Range('\t', '\u{000b}'));
  v.push(ClassItem::Literal('\r'));
  v.push(ClassItem::Literal(' '));
  v
});
