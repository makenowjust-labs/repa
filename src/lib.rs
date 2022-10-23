pub mod backtrack;
pub mod presburger;
pub mod regex;
pub mod solver;

use std::fmt::{self, Display};

use thiserror::Error;

use backtrack::{Program, ProgramBuilder};
use presburger::FormulaParser;
use regex::PatternParser;
use solver::NnfFormula;

pub use backtrack::Match;

#[derive(Clone, Debug)]
pub struct Regex {
    program: Program,
    formula: NnfFormula,
}

#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum RegexError {
    #[error("Error on parsing a pattern: {0}")]
    PatternParsingError(regex::ParsingError),
    #[error("Error on parsing a formula: {0}")]
    FormulaParsingError(presburger::ParsingError),
}

impl Regex {
    pub fn compile(pattern: &str, formula: &str) -> Result<Regex, RegexError> {
        let p = PatternParser::new(pattern)
            .parse()
            .map_err(|err| RegexError::PatternParsingError(err))?;
        let f = FormulaParser::new(formula)
            .parse()
            .map_err(|err| RegexError::FormulaParsingError(err))?;

        let program = ProgramBuilder::new().compile(&p);
        let nnf_formula = NnfFormula::from(f);

        Ok(Regex {
            program: program,
            formula: nnf_formula,
        })
    }

    pub fn execute<'a>(&self, input: &'a str, start_offset: usize) -> Option<Match<'a>> {
        self.program.execute(input, &self.formula, start_offset)
    }
}

impl Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "program:")?;
        for line in self.program.to_string().lines() {
            write!(f, "  {}\n", line)?;
        }
        write!(f, "formula: {}", self.formula)
    }
}
