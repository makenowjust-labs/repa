//! Presburger arithmetic formula, term and its parser.
//!
//! # Syntax
//!
//! ```text
//! f := 'true' | 'false'               # logical constants
//!    | f 'and' f | f 'or' f | 'not' f # logical operations
//!    | 'for' 'all' xs '.' f           # "for all" quantifier
//!    | 'for' 'some' xs '.' f          # "for some" quantifier
//!    | t op t                         # comparison tests
//!    | k '|' t | '-' k '|' t          # divisible test
//!    | '(' f ')'                      # group
//!
//! xs := x | x ',' xs
//! op ::= '=' | '!='
//!      | '<' | '>' | '<=' | '>='
//!
//! t := k t | k '*' t                  # multiplication
//!    | t '+' t | t '-' t              # addition and subtraction
//!    | '+' t | '-' t                  # unary operators
//!    | k                              # constant
//!    | x                              # variable
//!    | '(' t ')'                      # group
//!
//! k ::= [0-9]+
//! x ::= [a-zA-Z] [0-9a-zA-Z]*
//! ```
//!
//! Notes:
//!
//!   - A variables name cannot be reserved names in Presburger arithmetic formula (e.g. `for`, `all` and `true`)
//!   - A number constant cannot be followed by variables and reserved names without whitespaces.

use std::cell::Cell;
use std::fmt::Display;

use num_bigint::BigInt;
use thiserror::Error;

/// A Presburger arithmetic formula.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Formula {
    True,
    False,
    Comparison(ComparisonOp, Term, Term),
    Divisible(BigInt, Term),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    Not(Box<Formula>),
    ForAll(Vec<String>, Box<Formula>),
    ForSome(Vec<String>, Box<Formula>),
}

/// A Presburger arithmetic term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Const(BigInt),
    Var(String),
    Add(Box<Term>, Box<Term>),
    Sub(Box<Term>, Box<Term>),
    Mul(BigInt, Box<Term>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum FormulaOrTerm {
    Formula(Formula),
    Term(Term),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ComparisonOp {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct Token {
    kind: TokenKind,
    offset: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    For,
    All,
    Some,
    Dot,
    Comma,
    And,
    Or,
    True,
    False,
    Not,
    Plus,
    Minus,
    Asterisk,
    LeftParen,
    RightParen,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Divisible,
    Ident(String),
    Number(BigInt),
    EndOfString,
    Uninitialized,
}

impl TokenKind {
    fn is_stop_mul(&self) -> bool {
        match self {
            TokenKind::RightParen
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::EndOfString
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::LessThan
            | TokenKind::GreaterThan
            | TokenKind::LessThanOrEqual
            | TokenKind::GreaterThanOrEqual
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Divisible => true,
            _ => false,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::For => write!(f, "for"),
            TokenKind::All => write!(f, "all"),
            TokenKind::Some => write!(f, "some"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Comma => write!(f, ","),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Not => write!(f, "not"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::LessThanOrEqual => write!(f, "<="),
            TokenKind::GreaterThanOrEqual => write!(f, ">="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Divisible => write!(f, "|"),
            TokenKind::Ident(name) => write!(f, "{}", name),
            TokenKind::Number(n) => write!(f, "{}", n),
            TokenKind::EndOfString => write!(f, "end-of-string"),
            TokenKind::Uninitialized => write!(f, "uninitialized"),
        }
    }
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ParsingError {
    #[error("'{1}' is expected")]
    ExpectedChar(usize, char),
    #[error("end-of-string is expected")]
    ExpectedEndOfString(usize),
    #[error("formula is expected")]
    ExpectedFormula(usize),
    #[error("term is expected")]
    ExpectedTerm(usize),
    #[error("number is invalid")]
    InvalidNumber(usize),
    #[error("'{1}' is unexpected")]
    UnexpectedChar(usize, char),
    #[error("end-of-string is unexpected")]
    UnexpectedEndOfString(usize),
    #[error("token `{1}` is unexpected")]
    UnexpectedToken(usize, TokenKind),
}

impl ParsingError {
    pub fn offset(&self) -> usize {
        match *self {
            ParsingError::ExpectedChar(offset, _) => offset,
            ParsingError::ExpectedEndOfString(offset) => offset,
            ParsingError::ExpectedFormula(offset) => offset,
            ParsingError::ExpectedTerm(offset) => offset,
            ParsingError::InvalidNumber(offset) => offset,
            ParsingError::UnexpectedChar(offset, _) => offset,
            ParsingError::UnexpectedEndOfString(offset) => offset,
            ParsingError::UnexpectedToken(offset, _) => offset,
        }
    }
}

#[derive(Clone)]
pub struct FormulaParser<'s> {
    source: &'s str,
    offset_cell: Cell<usize>,
    current_token: Token,
}

impl<'s> FormulaParser<'s> {
    pub fn new(source: &'s str) -> FormulaParser<'s> {
        FormulaParser {
            source: source,
            offset_cell: Cell::new(0),
            current_token: Token {
                kind: TokenKind::Uninitialized,
                offset: 0,
            },
        }
    }

    pub fn parse(&mut self) -> Result<Formula, ParsingError> {
        self.next_token()?;

        let error_offset = self.current_token.offset;
        let formula = match self.parse_for()? {
            FormulaOrTerm::Formula(formula) => formula,
            FormulaOrTerm::Term(_) => return Err(ParsingError::ExpectedFormula(error_offset)),
        };

        if !self.end_of_string() {
            return Err(ParsingError::ExpectedEndOfString(self.offset()));
        }

        Ok(formula)
    }

    fn parse_for(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        if self.current_token.kind != TokenKind::For {
            return self.parse_or();
        }
        self.next_token()?;

        if self.current_token.kind != TokenKind::All && self.current_token.kind != TokenKind::Some {
            return Err(ParsingError::UnexpectedToken(
                self.current_token.offset,
                self.current_token.kind.clone(),
            ));
        }

        let is_all = self.current_token.kind == TokenKind::All;
        self.next_token()?;

        let mut names = vec![self.parse_ident()?];
        while self.current_token.kind == TokenKind::Comma {
            self.next_token()?;
            names.push(self.parse_ident()?);
        }

        if self.current_token.kind != TokenKind::Dot {
            return Err(ParsingError::UnexpectedToken(
                self.current_token.offset,
                self.current_token.kind.clone(),
            ));
        }
        self.next_token()?;

        let error_offset = self.current_token.offset;
        let formula = match self.parse_for()? {
            FormulaOrTerm::Formula(formula) => {
                if is_all {
                    Formula::ForAll(names, Box::new(formula))
                } else {
                    Formula::ForSome(names, Box::new(formula))
                }
            }
            FormulaOrTerm::Term(_) => return Err(ParsingError::ExpectedFormula(error_offset)),
        };
        Ok(FormulaOrTerm::Formula(formula))
    }

    fn parse_ident(&mut self) -> Result<String, ParsingError> {
        match &self.current_token.kind {
            TokenKind::Ident(name) => {
                let x = name.clone();
                self.next_token()?;
                Ok(x)
            }
            _ => Err(ParsingError::UnexpectedToken(
                self.current_token.offset,
                self.current_token.kind.clone(),
            )),
        }
    }

    fn parse_or(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let mut formula = match self.parse_and()? {
            FormulaOrTerm::Formula(formula) => formula,
            FormulaOrTerm::Term(term) => return Ok(FormulaOrTerm::Term(term)),
        };

        while self.current_token.kind == TokenKind::Or {
            self.next_token()?;
            let error_offset = self.current_token.offset;
            formula = match self.parse_and()? {
                FormulaOrTerm::Formula(right) => Formula::Or(Box::new(formula), Box::new(right)),
                FormulaOrTerm::Term(_) => return Err(ParsingError::ExpectedFormula(error_offset)),
            };
        }

        Ok(FormulaOrTerm::Formula(formula))
    }

    fn parse_and(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let mut formula = match self.parse_not()? {
            FormulaOrTerm::Formula(formula) => formula,
            FormulaOrTerm::Term(term) => return Ok(FormulaOrTerm::Term(term)),
        };

        while self.current_token.kind == TokenKind::And {
            self.next_token()?;
            let error_offset = self.current_token.offset;
            formula = match self.parse_not()? {
                FormulaOrTerm::Formula(right) => Formula::And(Box::new(formula), Box::new(right)),
                FormulaOrTerm::Term(_) => return Err(ParsingError::ExpectedFormula(error_offset)),
            };
        }

        Ok(FormulaOrTerm::Formula(formula))
    }

    fn parse_not(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        if self.current_token.kind != TokenKind::Not {
            return self.parse_comparison();
        }
        self.next_token()?;

        let error_offset = self.current_token.offset;
        match self.parse_not()? {
            FormulaOrTerm::Formula(formula) => {
                Ok(FormulaOrTerm::Formula(Formula::Not(Box::new(formula))))
            }
            FormulaOrTerm::Term(_) => Err(ParsingError::ExpectedFormula(error_offset)),
        }
    }

    fn parse_comparison(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let left = match self.parse_divisible()? {
            FormulaOrTerm::Formula(formula) => return Ok(FormulaOrTerm::Formula(formula)),
            FormulaOrTerm::Term(term) => term,
        };

        let op = match self.current_token.kind {
            TokenKind::LessThan => ComparisonOp::LessThan,
            TokenKind::GreaterThan => ComparisonOp::GreaterThan,
            TokenKind::LessThanOrEqual => ComparisonOp::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual => ComparisonOp::GreaterThanOrEqual,
            TokenKind::Equal => ComparisonOp::Equal,
            TokenKind::NotEqual => ComparisonOp::NotEqual,
            _ => return Ok(FormulaOrTerm::Term(left)),
        };
        self.next_token()?;

        let error_offset = self.current_token.offset;
        let right = match self.parse_divisible()? {
            FormulaOrTerm::Formula(_) => return Err(ParsingError::ExpectedTerm(error_offset)),
            FormulaOrTerm::Term(term) => term,
        };

        Ok(FormulaOrTerm::Formula(Formula::Comparison(op, left, right)))
    }

    fn parse_divisible(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let k = match self.parse_add()? {
            FormulaOrTerm::Formula(formula) => return Ok(FormulaOrTerm::Formula(formula)),
            FormulaOrTerm::Term(Term::Const(k)) => k,
            FormulaOrTerm::Term(term) => return Ok(FormulaOrTerm::Term(term)),
        };

        if self.current_token.kind != TokenKind::Divisible {
            return Ok(FormulaOrTerm::Term(Term::Const(k)));
        }
        self.next_token()?;

        let error_offset = self.current_token.offset;
        match self.parse_add()? {
            FormulaOrTerm::Formula(_) => Err(ParsingError::ExpectedTerm(error_offset)),
            FormulaOrTerm::Term(term) => Ok(FormulaOrTerm::Formula(Formula::Divisible(k, term))),
        }
    }

    fn parse_add(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let mut term = match self.parse_mul()? {
            FormulaOrTerm::Formula(formula) => return Ok(FormulaOrTerm::Formula(formula)),
            FormulaOrTerm::Term(term) => term,
        };

        while self.current_token.kind == TokenKind::Plus
            || self.current_token.kind == TokenKind::Minus
        {
            let is_plus = self.current_token.kind == TokenKind::Plus;
            self.next_token()?;
            let error_offset = self.current_token.offset;
            term = match self.parse_mul()? {
                FormulaOrTerm::Formula(_) => return Err(ParsingError::ExpectedTerm(error_offset)),
                FormulaOrTerm::Term(right) => {
                    if is_plus {
                        Term::Add(Box::new(term), Box::new(right))
                    } else {
                        Term::Sub(Box::new(term), Box::new(right))
                    }
                }
            };
        }

        Ok(FormulaOrTerm::Term(term))
    }

    fn parse_mul(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let k = match self.parse_prefix()? {
            FormulaOrTerm::Formula(formula) => return Ok(FormulaOrTerm::Formula(formula)),
            FormulaOrTerm::Term(Term::Const(k)) => k,
            FormulaOrTerm::Term(term) => return Ok(FormulaOrTerm::Term(term)),
        };

        if self.current_token.kind.is_stop_mul() {
            return Ok(FormulaOrTerm::Term(Term::Const(k)));
        }

        if self.current_token.kind == TokenKind::Asterisk {
            self.next_token()?;
        }

        let error_offset = self.current_token.offset;
        match self.parse_mul()? {
            FormulaOrTerm::Formula(_) => Err(ParsingError::ExpectedTerm(error_offset)),
            FormulaOrTerm::Term(term) => Ok(FormulaOrTerm::Term(Term::Mul(k, Box::new(term)))),
        }
    }

    fn parse_prefix(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        let error_offset = self.current_token.offset;
        match self.current_token.kind {
            TokenKind::Plus => {
                self.next_token()?;
                match self.parse_prefix()? {
                    FormulaOrTerm::Formula(_) => Err(ParsingError::ExpectedTerm(error_offset)),
                    FormulaOrTerm::Term(term) => Ok(FormulaOrTerm::Term(term)),
                }
            }
            TokenKind::Minus => {
                self.next_token()?;
                match self.parse_prefix()? {
                    FormulaOrTerm::Formula(_) => Err(ParsingError::ExpectedTerm(error_offset)),
                    FormulaOrTerm::Term(Term::Const(k)) => Ok(FormulaOrTerm::Term(Term::Const(-k))),
                    FormulaOrTerm::Term(term) => Ok(FormulaOrTerm::Term(Term::Mul(
                        BigInt::from(-1),
                        Box::new(term),
                    ))),
                }
            }
            _ => self.parse_atom(),
        }
    }

    fn parse_atom(&mut self) -> Result<FormulaOrTerm, ParsingError> {
        match &self.current_token.kind {
            TokenKind::Ident(name) => {
                let term = Term::Var(name.clone());
                self.next_token()?;
                Ok(FormulaOrTerm::Term(term))
            }
            TokenKind::Number(k) => {
                let term = Term::Const(k.clone());
                self.next_token()?;
                Ok(FormulaOrTerm::Term(term))
            }
            TokenKind::True => {
                self.next_token()?;
                Ok(FormulaOrTerm::Formula(Formula::True))
            }
            TokenKind::False => {
                self.next_token()?;
                Ok(FormulaOrTerm::Formula(Formula::False))
            }
            TokenKind::LeftParen => {
                self.next_token()?;
                let formula_or_term = self.parse_or()?;
                if self.current_token.kind != TokenKind::RightParen {
                    return Err(ParsingError::ExpectedChar(self.current_token.offset, ')'));
                }
                self.next_token()?;
                Ok(formula_or_term)
            }
            kind => Err(ParsingError::UnexpectedToken(
                self.current_token.offset,
                kind.clone(),
            )),
        }
    }

    fn next_token(&mut self) -> Result<(), ParsingError> {
        if self.current_token.kind == TokenKind::EndOfString {
            return Ok(());
        }

        self.skip_space()?;
        let offset = self.offset();

        if self.end_of_string() {
            self.set_current_token(offset, TokenKind::EndOfString);
            return Ok(());
        }

        let c = self.current_char()?;

        if c.is_ascii_digit() {
            self.next_number_token()?;
            return Ok(());
        }

        if c.is_ascii_alphabetic() || c == '_' {
            self.next_ident_token()?;
            return Ok(());
        }

        if c == '<' {
            self.next_char();
            if !self.end_of_string() && self.current_char()? == '=' {
                self.next_char();
                self.set_current_token(offset, TokenKind::LessThanOrEqual);
            } else {
                self.set_current_token(offset, TokenKind::LessThan);
            }
            return Ok(());
        }

        if c == '>' {
            self.next_char();
            if !self.end_of_string() && self.current_char()? == '=' {
                self.next_char();
                self.set_current_token(offset, TokenKind::GreaterThanOrEqual);
            } else {
                self.set_current_token(offset, TokenKind::GreaterThan);
            }
            return Ok(());
        }

        if c == '!' {
            self.next_char();
            if self.current_char()? != '=' {
                return Err(ParsingError::ExpectedChar(offset, '='));
            }
            self.next_char();
            self.set_current_token(offset, TokenKind::NotEqual);
            return Ok(());
        }

        match c {
            '.' => self.set_current_token(offset, TokenKind::Dot),
            ',' => self.set_current_token(offset, TokenKind::Comma),
            '+' => self.set_current_token(offset, TokenKind::Plus),
            '-' => self.set_current_token(offset, TokenKind::Minus),
            '*' => self.set_current_token(offset, TokenKind::Asterisk),
            '(' => self.set_current_token(offset, TokenKind::LeftParen),
            ')' => self.set_current_token(offset, TokenKind::RightParen),
            '=' => self.set_current_token(offset, TokenKind::Equal),
            '|' => self.set_current_token(offset, TokenKind::Divisible),
            c => return Err(ParsingError::UnexpectedChar(offset, c)),
        }
        self.next_char();

        Ok(())
    }

    fn skip_space(&self) -> Result<(), ParsingError> {
        while !self.end_of_string() && self.current_char()?.is_ascii_whitespace() {
            self.next_char();
        }
        Ok(())
    }

    fn next_number_token(&mut self) -> Result<(), ParsingError> {
        let start_offset = self.offset();
        let mut c = self.current_char()?;
        while !self.end_of_string() && (c.is_ascii_alphanumeric() || c == '_') {
            self.next_char();
            if !self.end_of_string() {
                c = self.current_char()?;
            }
        }
        let end_offset = self.offset();

        let n = match BigInt::parse_bytes(self.source[start_offset..end_offset].as_bytes(), 10) {
            Some(n) => n,
            None => return Err(ParsingError::InvalidNumber(start_offset)),
        };
        self.set_current_token(start_offset, TokenKind::Number(n));
        Ok(())
    }

    fn next_ident_token(&mut self) -> Result<(), ParsingError> {
        let start_offset = self.offset();
        let mut c = self.current_char()?;
        while !self.end_of_string() && (c.is_ascii_alphanumeric() || c == '_') {
            self.next_char();
            if !self.end_of_string() {
                c = self.current_char()?;
            }
        }
        let end_offset = self.offset();

        let name = self.source[start_offset..end_offset].to_string();
        let kind = match name.as_str() {
            "for" => TokenKind::For,
            "all" => TokenKind::All,
            "some" => TokenKind::Some,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(name),
        };
        self.set_current_token(start_offset, kind);
        Ok(())
    }

    fn set_current_token(&mut self, offset: usize, kind: TokenKind) {
        self.current_token = Token {
            kind: kind,
            offset: offset,
        };
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
}

#[cfg(test)]
fn parse(source: &str) -> Result<Formula, ParsingError> {
    FormulaParser::new(source).parse()
}

#[cfg(test)]
fn parse_term(source: &str) -> Result<Term, ParsingError> {
    let mut parser = FormulaParser::new(source);
    parser.next_token()?;

    let error_offset = parser.current_token.offset;
    let term = match parser.parse_add()? {
        FormulaOrTerm::Formula(_) => return Err(ParsingError::ExpectedTerm(error_offset)),
        FormulaOrTerm::Term(term) => term,
    };

    if !parser.end_of_string() {
        return Err(ParsingError::ExpectedEndOfString(parser.offset()));
    }

    Ok(term)
}

#[test]
fn test_parse_for() {
    assert_eq!(
        parse("for all x. x = 1"),
        Ok(Formula::ForAll(
            vec!["x".to_string()],
            Box::new(Formula::Comparison(
                ComparisonOp::Equal,
                Term::Var("x".to_string()),
                Term::Const(BigInt::from(1))
            ))
        ))
    );
    assert_eq!(
        parse("for all x, y. x = y"),
        Ok(Formula::ForAll(
            vec!["x".to_string(), "y".to_string()],
            Box::new(Formula::Comparison(
                ComparisonOp::Equal,
                Term::Var("x".to_string()),
                Term::Var("y".to_string())
            ))
        ))
    );
    assert_eq!(
        parse("for some x. x = 1"),
        Ok(Formula::ForSome(
            vec!["x".to_string()],
            Box::new(Formula::Comparison(
                ComparisonOp::Equal,
                Term::Var("x".to_string()),
                Term::Const(BigInt::from(1))
            ))
        ))
    );
}

#[test]
fn test_parse_and() {
    assert_eq!(
        parse("true and false"),
        Ok(Formula::And(
            Box::new(Formula::True),
            Box::new(Formula::False)
        ))
    );
}

#[test]
fn test_parse_or() {
    assert_eq!(
        parse("true or false"),
        Ok(Formula::Or(
            Box::new(Formula::True),
            Box::new(Formula::False)
        ))
    );
}

#[test]
fn test_parse_not() {
    assert_eq!(parse("not true"), Ok(Formula::Not(Box::new(Formula::True))));
}

#[test]
fn test_parse_comparison() {
    assert_eq!(
        parse("1 < 2"),
        Ok(Formula::Comparison(
            ComparisonOp::LessThan,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
    assert_eq!(
        parse("1 > 2"),
        Ok(Formula::Comparison(
            ComparisonOp::GreaterThan,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
    assert_eq!(
        parse("1 <= 2"),
        Ok(Formula::Comparison(
            ComparisonOp::LessThanOrEqual,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
    assert_eq!(
        parse("1 >= 2"),
        Ok(Formula::Comparison(
            ComparisonOp::GreaterThanOrEqual,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
    assert_eq!(
        parse("1 = 2"),
        Ok(Formula::Comparison(
            ComparisonOp::Equal,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
    assert_eq!(
        parse("1 != 2"),
        Ok(Formula::Comparison(
            ComparisonOp::NotEqual,
            Term::Const(BigInt::from(1)),
            Term::Const(BigInt::from(2))
        ))
    );
}

#[test]
fn test_parse_divisible() {
    assert_eq!(
        parse("1 | 2"),
        Ok(Formula::Divisible(
            BigInt::from(1),
            Term::Const(BigInt::from(2))
        ))
    );
}

#[test]
fn test_parse_add() {
    assert_eq!(
        parse_term("1 + 2"),
        Ok(Term::Add(
            Box::new(Term::Const(BigInt::from(1))),
            Box::new(Term::Const(BigInt::from(2)))
        ))
    );
    assert_eq!(
        parse_term("1 - 2"),
        Ok(Term::Sub(
            Box::new(Term::Const(BigInt::from(1))),
            Box::new(Term::Const(BigInt::from(2)))
        ))
    );
}

#[test]
fn test_parse_mul() {
    assert_eq!(
        parse_term("3 x"),
        Ok(Term::Mul(
            BigInt::from(3),
            Box::new(Term::Var("x".to_string()))
        ))
    );
    assert_eq!(
        parse_term("3 * x"),
        Ok(Term::Mul(
            BigInt::from(3),
            Box::new(Term::Var("x".to_string()))
        ))
    );
}

#[test]
fn test_parse_prefix() {
    assert_eq!(parse_term("+1"), Ok(Term::Const(BigInt::from(1))));
    assert_eq!(parse_term("-42"), Ok(Term::Const(BigInt::from(-42))));
    assert_eq!(parse_term("+foo"), Ok(Term::Var("foo".to_string())));
    assert_eq!(
        parse_term("-foo"),
        Ok(Term::Mul(
            BigInt::from(-1),
            Box::new(Term::Var("foo".to_string()))
        ))
    );
}

#[test]
fn test_parse_atom() {
    assert_eq!(parse("true"), Ok(Formula::True));
    assert_eq!(parse("false"), Ok(Formula::False));
    assert_eq!(parse_term("1"), Ok(Term::Const(BigInt::from(1))));
    assert_eq!(parse_term("42"), Ok(Term::Const(BigInt::from(42))));
    assert_eq!(parse_term("foo"), Ok(Term::Var("foo".to_string())));
    assert_eq!(parse("(true)"), Ok(Formula::True));
    assert_eq!(parse_term("(1)"), Ok(Term::Const(BigInt::from(1))));
}
