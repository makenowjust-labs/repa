//! Backtrack-based regular-expression matching engine.

use std::cell::Cell;
use std::collections::BTreeMap;
use std::fmt::{self, Display};
use std::rc::Rc;

use gcollections::ops::bounded::Bounded;
use gcollections::ops::constructor::Empty;
use gcollections::ops::set::{Contains, Difference, Union};
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
    SaveOffset(usize),
    NullCheck(usize),
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
    register: usize,
    value: usize,
    jump: usize,
}

#[derive(Clone, Debug)]
pub struct Program {
    ops: Vec<Op>,
    register_size: usize,
    capture_size: usize,
}

#[derive(Clone, Debug)]
pub struct ProgramBuilder {
    ops: Vec<Op>,
    next_register: Rc<Cell<usize>>,
    register_size: usize,
    capture_size: usize,
}

#[derive(Debug)]
struct Register(Rc<Cell<usize>>, usize);

impl<'a> std::ops::Deref for Register {
    type Target = usize;
    fn deref(&self) -> &usize {
        &self.1
    }
}

impl<'a> Drop for Register {
    fn drop(&mut self) {
        self.0.set(self.0.get() - 1);
    }
}

impl ProgramBuilder {
    pub fn new() -> ProgramBuilder {
        ProgramBuilder {
            ops: Vec::new(),
            next_register: Rc::new(Cell::new(0)),
            register_size: 0,
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
            register_size: self.register_size,
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
                self.compile_pattern_null_check(child);
                self.ops.push(Op::Jump(fork));
                self.ops[fork] = Op::Fork(self.ops.len());
            }
            Pattern::Repetition(child, RepetitionKind::OneOrMore)
            | Pattern::Repetition(child, RepetitionKind::AtLeast(1)) => {
                self.compile_pattern(child);
                let fork = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern_null_check(child);
                self.ops.push(Op::Jump(fork));
                self.ops[fork] = Op::Fork(self.ops.len());
            }
            Pattern::Repetition(_, RepetitionKind::Exactly(0)) => {}
            Pattern::Repetition(child, RepetitionKind::Exactly(1)) => {
                self.compile_pattern(child);
            }
            Pattern::Repetition(child, RepetitionKind::Exactly(n)) => {
                let register = self.next_register();
                self.ops.push(Op::ResetCounter(*register));
                let compare = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::IncrementCounter(*register));
                self.ops.push(Op::Jump(compare));
                self.ops[compare] = Op::Compare(Compare {
                    register: *register,
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
                let register = self.next_register();
                self.ops.push(Op::ResetCounter(*register));
                let compare1 = self.ops.len();
                self.ops.push(Op::Dummy);
                self.compile_pattern(child);
                self.ops.push(Op::IncrementCounter(*register));
                self.ops.push(Op::Jump(compare1));
                self.ops[compare1] = Op::Compare(Compare {
                    register: *register,
                    value: *n,
                    jump: self.ops.len(),
                });
                let compare2 = self.ops.len();
                self.ops.push(Op::Dummy);
                self.ops.push(Op::Dummy);
                self.compile_pattern_null_check(child);
                self.ops.push(Op::IncrementCounter(*register));
                self.ops.push(Op::Jump(compare2));
                self.ops[compare2] = Op::Compare(Compare {
                    register: *register,
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
                self.capture_size = self.capture_size.max(i * 2 + 2);
                self.ops.push(Op::Record(i * 2));
                self.compile_pattern(child);
                self.ops.push(Op::Record(i * 2 + 1));
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

    fn compile_pattern_null_check(&mut self, pattern: &Pattern) {
        if !can_be_empty(pattern) {
            self.compile_pattern(pattern);
            return;
        }

        let register = self.next_register();
        self.ops.push(Op::SaveOffset(*register));
        self.compile_pattern(pattern);
        self.ops.push(Op::NullCheck(*register));
    }

    fn next_register(&mut self) -> Register {
        let register = self.next_register.get();
        if self.register_size == register {
            self.register_size += 1;
        }
        self.next_register.set(register + 1);
        Register(Rc::clone(&self.next_register), register)
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
                    subset = (0 as u32, char::MAX as u32)
                        .to_interval_set()
                        .difference(&subset);
                }
                set = set.union(&subset);
            }
        }
    }
    set
}

fn can_be_empty(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Alternation(children) => children.iter().any(|p| can_be_empty(p)),
        Pattern::Concat(children) => children.iter().all(|p| can_be_empty(p)),
        Pattern::Repetition(child, kind) => match kind {
            RepetitionKind::ZeroOrOne
            | RepetitionKind::ZeroOrMore
            | RepetitionKind::Exactly(0)
            | RepetitionKind::AtLeast(0)
            | RepetitionKind::Bounded(0, _) => true,
            _ => can_be_empty(child),
        },
        Pattern::Assertion(_) | Pattern::Increment(_) => true,
        Pattern::Group(child) | Pattern::Capture(_, child) => can_be_empty(child),
        Pattern::Dot | Pattern::Literal(_) | Pattern::Class(_, _) => false,
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Dummy => write!(f, "dummy"),
            Op::Match => write!(f, "match"),
            Op::Jump(addr) => write!(f, "jump\t\t@{:04}", addr),
            Op::Fork(addr) => write!(f, "fork\t\t@{:04}", addr),
            Op::Assert(k) => match k {
                AssertionKind::StartLine => write!(f, "assert\t\tstart-line"),
                AssertionKind::EndLine => write!(f, "assert\t\tend-line"),
                AssertionKind::StartText => write!(f, "assert\t\tstart-text"),
                AssertionKind::EndText => write!(f, "assert\t\tend-text"),
                AssertionKind::WordBoundary => write!(f, "assert\t\tword-boundary"),
                AssertionKind::NotWordBoundary => write!(f, "assert\t\tnot-word-boundary"),
            },
            Op::SaveOffset(reg) => write!(f, "save-offset\t${}", reg),
            Op::NullCheck(reg) => write!(f, "null-check\t${}", reg),
            Op::ResetCounter(reg) => write!(f, "reset-counter\t${}", reg),
            Op::IncrementCounter(reg) => write!(f, "inc-counter\t${}", reg),
            Op::Compare(Compare {
                register,
                value,
                jump,
            }) => write!(f, "compare\t\t${} >= {}\t@{:04}", register, value, jump),
            Op::Literal(c) => write!(f, "literal\t\t'{}'", c.escape_debug()),
            Op::Class(Class { not, set }) => {
                fn write_u32(f: &mut std::fmt::Formatter<'_>, x: u32) -> fmt::Result {
                    match char::from_u32(x) {
                        Some(c) => match c {
                            '-' | ']' => write!(f, "\\{}", c),
                            _ => write!(f, "{}", c.escape_debug()),
                        },
                        None => write!(f, "\\u{:04x}", x),
                    }
                }
                write!(f, "class\t\t[")?;
                if *not {
                    write!(f, "^")?;
                }
                for i in set.iter() {
                    let (l, r) = (i.lower(), i.upper());
                    if l == r {
                        write_u32(f, l)?;
                    } else if l + 1 == r {
                        write_u32(f, l)?;
                        write_u32(f, r)?;
                    } else {
                        write_u32(f, l)?;
                        write!(f, "-")?;
                        write_u32(f, r)?;
                    }
                }
                write!(f, "]")
            }
            Op::Dot => write!(f, "dot"),
            Op::Record(cap) => write!(f, "record\t\t&{}", cap),
            Op::IncrementVar(v) => write!(f, "inc-var\t\t{}", v.to_string()),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, op) in self.ops.iter().enumerate() {
            writeln!(f, "@{:04}: {}", i, op)?;
        }
        Ok(())
    }
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
    register: Vec<usize>,
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
        let mut register = vec![usize::MAX; self.register_size];
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
                        register: register.clone(),
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
                Op::SaveOffset(x) => {
                    register[*x] = offset;
                    true
                }
                Op::NullCheck(x) => register[*x] != offset,
                Op::ResetCounter(x) => {
                    register[*x] = 0;
                    true
                }
                Op::IncrementCounter(x) => {
                    register[*x] += 1;
                    true
                }
                Op::Compare(cmp) => {
                    if register[cmp.register] >= cmp.value {
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
                        register = item.register;
                        capture = item.capture;
                        valuation = item.valuation;
                    }
                    None => {
                        if offset == input.len() {
                            return None;
                        }
                        offset += 1;
                        pc = 0;
                        register = vec![usize::MAX; self.register_size];
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

#[cfg(test)]
fn execute_pattern(p: &str, f: &str, input: &str) -> bool {
    let pattern = parse_pattern(p);
    let formula = parse_formula(f);
    let program = ProgramBuilder::new().compile(&pattern);
    let nnf_formula = NnfFormula::from(formula);

    program.execute(input, &nnf_formula, 0).is_some()
}

#[test]
fn test_can_be_empty() {
    assert_eq!(can_be_empty(&parse_pattern("")), true);
    assert_eq!(can_be_empty(&parse_pattern("a")), false);
    assert_eq!(can_be_empty(&parse_pattern("[ab]")), false);
    assert_eq!(can_be_empty(&parse_pattern("\\b")), true);
    assert_eq!(can_be_empty(&parse_pattern("(a)*")), true);
    assert_eq!(can_be_empty(&parse_pattern("(a)+")), false);
    assert_eq!(can_be_empty(&parse_pattern("a|b")), false);
    assert_eq!(can_be_empty(&parse_pattern("a|")), true);
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

    // literal:
    assert_eq!(execute_pattern("x", "true", "x"), true);
    assert_eq!(execute_pattern("x", "true", "y"), false);

    // class:
    assert_eq!(execute_pattern("[xy]", "true", "x"), true);
    assert_eq!(execute_pattern("[xy]", "true", "y"), true);
    assert_eq!(execute_pattern("[xy]", "true", "z"), false);
    assert_eq!(execute_pattern("[^xy]", "true", "z"), true);

    // concat:
    assert_eq!(execute_pattern("xy", "true", "xy"), true);
    assert_eq!(execute_pattern("xy", "true", "yz"), false);

    // alternation:
    assert_eq!(execute_pattern("x|y", "true", "x"), true);
    assert_eq!(execute_pattern("x|y", "true", "y"), true);
    assert_eq!(execute_pattern("x|y", "true", "z"), false);

    // repetition:
    assert_eq!(execute_pattern("^a?$", "true", ""), true);
    assert_eq!(execute_pattern("^a?$", "true", "a"), true);
    assert_eq!(execute_pattern("^a?$", "true", "aa"), false);
    assert_eq!(execute_pattern("^a*$", "true", ""), true);
    assert_eq!(execute_pattern("^a*$", "true", "a"), true);
    assert_eq!(execute_pattern("^a*$", "true", "aaa"), true);
    assert_eq!(execute_pattern("^a*$", "true", ""), true);
    assert_eq!(execute_pattern("^a*$", "true", "a"), true);
    assert_eq!(execute_pattern("^a*$", "true", "b"), false);
    assert_eq!(execute_pattern("^a*$", "true", "aaa"), true);
    assert_eq!(execute_pattern("^a*$", "true", "aba"), false);
    assert_eq!(execute_pattern("^a+$", "true", ""), false);
    assert_eq!(execute_pattern("^a+$", "true", "a"), true);
    assert_eq!(execute_pattern("^a+$", "true", "b"), false);
    assert_eq!(execute_pattern("^a+$", "true", "aaa"), true);
    assert_eq!(execute_pattern("^a+$", "true", "aba"), false);
    assert_eq!(execute_pattern("^a{2}$", "true", ""), false);
    assert_eq!(execute_pattern("^a{2}$", "true", "a"), false);
    assert_eq!(execute_pattern("^a{2}$", "true", "aa"), true);
    assert_eq!(execute_pattern("^a{2}$", "true", "aaa"), false);
    assert_eq!(execute_pattern("^a{2,}$", "true", ""), false);
    assert_eq!(execute_pattern("^a{2,}$", "true", "a"), false);
    assert_eq!(execute_pattern("^a{2,}$", "true", "aa"), true);
    assert_eq!(execute_pattern("^a{2,}$", "true", "aaa"), true);
    assert_eq!(execute_pattern("^a{2,3}$", "true", ""), false);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "a"), false);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "b"), false);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "aa"), true);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "aaa"), true);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "aba"), false);
    assert_eq!(execute_pattern("^a{2,3}$", "true", "aaaa"), false);

    // assertion:
    assert_eq!(execute_pattern("aa\n^", "true", "aa\n"), true);
    assert_eq!(execute_pattern("aa$", "true", "aa\n"), true);
    assert_eq!(execute_pattern("\\A", "true", ""), true);
    assert_eq!(execute_pattern("\\z", "true", ""), true);
    assert_eq!(execute_pattern("a\\b ", "true", "a "), true);
    assert_eq!(execute_pattern("a\\Ba", "true", "aa"), true);
}
