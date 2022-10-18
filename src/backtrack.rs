//! Backtrack-based regular-expression matching engine.

use std::collections::BTreeMap;

use gcollections::ops::constructor::Empty;
use gcollections::ops::set::{Complement, Contains, Union};
use interval::interval_set::{IntervalSet, ToIntervalSet};
use num_traits::Zero;

use crate::presburger::{Name, Z};
use crate::regex::{AssertionKind, ClassItem, Pattern, RepetitionKind};
use crate::solver::NnfFormula;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Op {
    Dummy,
    Match,
    Jump(usize),
    Fork(usize),
    Assert(AssertionKind),
    ResetCounter(usize),
    IncrementCounter(usize),
    Compare(Compare),
    Literal(char),
    Dot,
    Class(Class),
    Record(usize),
    IncrementVar(Name),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Class {
    not: bool,
    set: IntervalSet<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Compare {
    counter: usize,
    value: u32,
    jump: usize,
}

#[derive(Clone, Debug)]
pub struct Program {
    ops: Vec<Op>,
    counter_size: usize,
    capture_size: usize,
}

#[derive(Clone, Debug)]
pub struct ProgramBuilder {
    ops: Vec<Op>,
    counter_size: usize,
    capture_size: usize,
}

impl ProgramBuilder {
    pub fn new() -> ProgramBuilder {
        ProgramBuilder {
            ops: Vec::new(),
            counter_size: 0,
            capture_size: 2,
        }
    }

    pub fn compile(&mut self, pattern: &Pattern) -> Program {
        self.ops.push(Op::Record(0));
        self.compile_pattern(pattern);
        self.ops.push(Op::Record(1));
        self.ops.push(Op::Match);

        Program {
            ops: self.ops.clone(),
            counter_size: self.counter_size,
            capture_size: self.capture_size,
        }
    }

    fn compile_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Alternation(children) => {
                let mut jumps = Vec::new();
                for child in &children[0..children.len() - 1] {
                    let fork = self.ops.len();
                    self.ops.push(Op::Dummy);
                    self.compile_pattern(child);
                    jumps.push(self.ops.len());
                    self.ops.push(Op::Dummy);
                    self.ops[fork] = Op::Fork(self.ops.len());
                }

                let child = &children[children.len() - 1];
                self.compile_pattern(child);
                for jump in jumps {
                    self.ops[jump] = Op::Jump(self.ops.len());
                }
            }
            Pattern::Concat(children) => {
                for child in children {
                    self.compile_pattern(child);
                }
            }
            Pattern::Repetition(child, RepetitionKind::ZeroOrOne)
            | Pattern::Repetition(child, RepetitionKind::Bounded(0, 1)) => {
                let fork = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops[fork] = Op::Fork(self.ops.len());
            }
            Pattern::Repetition(child, RepetitionKind::ZeroOrMore)
            | Pattern::Repetition(child, RepetitionKind::AtLeast(0)) => {
                let fork = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::Jump(fork));
                self.ops[fork] = Op::Fork(self.ops.len());
            }
            Pattern::Repetition(child, RepetitionKind::OneOrMore)
            | Pattern::Repetition(child, RepetitionKind::AtLeast(1)) => {
                let start = self.ops.len();
                self.compile_pattern(child);
                self.ops.push(Op::Fork(self.ops.len() + 2));
                self.ops.push(Op::Jump(start));
            }
            Pattern::Repetition(child, RepetitionKind::Exactly(n)) => {
                let counter = self.counter_size;
                self.counter_size += 1;
                self.ops.push(Op::ResetCounter(counter));
                let compare = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::IncrementCounter(counter));
                self.ops.push(Op::Jump(compare));
                self.ops[compare] = Op::Compare(Compare {
                    counter: counter,
                    value: *n,
                    jump: self.ops.len(),
                });
            }
            Pattern::Repetition(child, RepetitionKind::AtLeast(n)) => {
                self.compile_pattern(&Pattern::Repetition(
                    child.clone(),
                    RepetitionKind::Exactly(*n),
                ));
                self.compile_pattern(&Pattern::Repetition(
                    child.clone(),
                    RepetitionKind::ZeroOrMore,
                ));
            }
            Pattern::Repetition(child, RepetitionKind::Bounded(n, m)) => {
                let counter = self.counter_size;
                self.counter_size += 1;
                self.ops.push(Op::ResetCounter(counter));
                let compare1 = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::IncrementCounter(counter));
                self.ops.push(Op::Jump(compare1));
                self.ops[compare1] = Op::Compare(Compare {
                    counter: counter,
                    value: *n,
                    jump: self.ops.len(),
                });
                let compare2 = self.ops.len();
                self.ops.push(Op::Dummy);
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::IncrementCounter(counter));
                self.ops.push(Op::Jump(compare2));
                self.ops[compare2] = Op::Compare(Compare {
                    counter: counter,
                    value: *m,
                    jump: self.ops.len(),
                });
                self.ops[compare2 + 1] = Op::Fork(self.ops.len());
            }
            Pattern::Assertion(k) => self.ops.push(Op::Assert(k.clone())),
            Pattern::Group(child) => {
                self.compile_pattern(child);
            }
            Pattern::Capture(i, child) => {
                self.capture_size = self.capture_size.max((i + 1) * 2 + 2);
                self.ops.push(Op::Record((i + 1) * 2));
                self.compile_pattern(child);
                self.ops.push(Op::Record((i + 1) * 2 + 1));
            }
            Pattern::Dot => self.ops.push(Op::Dot),
            Pattern::Literal(c) => self.ops.push(Op::Literal(*c)),
            Pattern::Class(not, items) => self.ops.push(Op::Class(Class {
                not: *not,
                set: items_to_interval_set(items),
            })),
            Pattern::Increment(x) => self.ops.push(Op::IncrementVar(x.clone())),
        }
    }
}

/// Converts the given items to an interval set.
fn items_to_interval_set(items: &Vec<ClassItem>) -> IntervalSet<u32> {
    let mut set = IntervalSet::empty();
    for item in items {
        match item {
            ClassItem::Literal(c) => set = set.union(&(*c as u32, *c as u32).to_interval_set()),
            ClassItem::Range(l, r) => set = set.union(&(*l as u32, *r as u32).to_interval_set()),
            ClassItem::Class(not, items) => {
                let mut subset = items_to_interval_set(items);
                if *not {
                    subset = subset.complement();
                }
                set = set.union(&subset);
            }
        }
    }
    set
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Match<'a> {
    pub input: &'a str,
    pub capture: Vec<usize>,
    pub valuation: BTreeMap<Name, Z>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct StackItem {
    offset: usize,
    pc: usize,
    counter: Vec<u32>,
    capture: Vec<usize>,
    valuation: BTreeMap<Name, Z>,
}

impl Program {
    pub fn execute<'a>(
        &self,
        input: &'a str,
        formula: &NnfFormula,
        start_offset: usize,
    ) -> Option<Match<'a>> {
        let mut stack = Vec::new();
        let mut offset = start_offset;
        let mut pc = 0;
        let mut counter = vec![u32::MAX; self.counter_size];
        let mut capture = vec![usize::MAX; self.capture_size];
        let mut valuation = BTreeMap::new();

        loop {
            let ok = match &self.ops[pc] {
                Op::Dummy => panic!("BUG: dummy"),
                Op::Match => {
                    if formula.check(&valuation) {
                        return Some(Match {
                            input: input,
                            capture: capture,
                            valuation: valuation,
                        });
                    }
                    false
                }
                Op::Jump(new_pc) => {
                    pc = *new_pc;
                    continue;
                }
                Op::Fork(next_pc) => {
                    stack.push(StackItem {
                        offset: offset,
                        pc: *next_pc,
                        counter: counter.clone(),
                        capture: capture.clone(),
                        valuation: valuation.clone(),
                    });
                    true
                }
                Op::Assert(k) => match k {
                    AssertionKind::StartText => offset == 0,
                    AssertionKind::EndText => offset == input.len(),
                    AssertionKind::StartLine => {
                        offset == 0 || {
                            let prev = input.as_bytes()[offset - 1] as char;
                            prev == '\n' || prev == '\r'
                        }
                    }
                    AssertionKind::EndLine => {
                        offset == input.len() || {
                            let curr = input.as_bytes()[offset] as char;
                            curr == '\n' || curr == '\r'
                        }
                    }
                    AssertionKind::WordBoundary | AssertionKind::NotWordBoundary => {
                        let prev_is_word = offset != 0 && {
                            let prev = input.as_bytes()[offset - 1] as char;
                            prev.is_ascii_alphanumeric() || prev == '_'
                        };
                        let curr_is_word = offset != input.len() && {
                            let prev = input.as_bytes()[offset] as char;
                            prev.is_ascii_alphanumeric() || prev == '_'
                        };

                        (prev_is_word != curr_is_word) == (k == &AssertionKind::WordBoundary)
                    }
                },
                Op::ResetCounter(x) => {
                    counter[*x] = 0;
                    true
                }
                Op::IncrementCounter(x) => {
                    counter[*x] += 1;
                    true
                }
                Op::Compare(cmp) => {
                    if counter[cmp.counter] >= cmp.value {
                        pc = cmp.jump;
                        continue;
                    }
                    true
                }
                Op::Literal(c) => {
                    if offset < input.len() {
                        let curr = input[offset..].chars().next().unwrap();
                        if curr == *c {
                            offset += curr.len_utf8();
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Op::Dot => {
                    if offset < input.len() {
                        let curr = input[offset..].chars().next().unwrap();
                        offset += curr.len_utf8();
                        true
                    } else {
                        false
                    }
                }
                Op::Class(cls) => {
                    if offset < input.len() {
                        let curr = input[offset..].chars().next().unwrap();
                        if cls.not == !cls.set.contains(&(curr as u32)) {
                            offset += curr.len_utf8();
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                Op::Record(i) => {
                    capture[*i] = offset;
                    true
                }
                Op::IncrementVar(name) => {
                    *valuation.entry(name.clone()).or_insert(Zero::zero()) += 1;
                    true
                }
            };

            if ok {
                pc += 1;
            } else {
                match stack.pop() {
                    Some(item) => {
                        offset = item.offset;
                        pc = item.pc;
                        counter = item.counter;
                        capture = item.capture;
                        valuation = item.valuation;
                    }
                    None => {
                        if offset == input.len() {
                            return None;
                        }
                        offset += 1;
                        pc = 0;
                        counter = vec![u32::MAX; self.counter_size];
                        capture = vec![usize::MAX; self.capture_size];
                        valuation = BTreeMap::new();
                    }
                }
            }
        }
    }
}

#[cfg(test)]
fn parse_formula(s: &str) -> crate::presburger::Formula {
    use crate::presburger::FormulaParser;
    FormulaParser::new(s).parse().unwrap()
}

#[cfg(test)]
fn parse_pattern(s: &str) -> Pattern {
    use crate::regex::PatternParser;
    PatternParser::new(s).parse().unwrap()
}

#[test]
fn test_execute() {
    let pattern = parse_pattern(r"\A(?:x{x}|y{y}|z{z})*\z");
    let formula = parse_formula("x = y and y = z");
    let program = ProgramBuilder::new().compile(&pattern);
    let nnf_formula = NnfFormula::from(formula);

    assert_eq!(
        program.execute("", &nnf_formula, 0),
        Some(Match {
            input: "",
            capture: vec![0, 0],
            valuation: BTreeMap::new(),
        })
    );
    assert_eq!(program.execute("x", &nnf_formula, 0), None);
    assert_eq!(program.execute("y", &nnf_formula, 0), None);
    assert_eq!(program.execute("z", &nnf_formula, 0), None);
    assert_eq!(
        program.execute("xyzyzx", &nnf_formula, 0),
        Some(Match {
            input: "xyzyzx",
            capture: vec![0, 6],
            valuation: BTreeMap::from([
                ("x".into(), 2.into()),
                ("y".into(), 2.into()),
                ("z".into(), 2.into())
            ]),
        })
    );
}
