pub mod backtrack;
pub mod presburger;
pub mod regex;
pub mod solver;

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
    #[error(transparent)]
    PatternParsingError(regex::ParsingError),
    #[error(transparent)]
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

    pub fn execute<'a>(&self, input: &'a str) -> Option<Match<'a>> {
        self.program.execute(input, &self.formula)
    }
}
