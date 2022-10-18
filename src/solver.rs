//! Presburger arithmetic formula solver.
//!
//! This solver uses Cooper's algorithm.

use std::collections::{BTreeMap, HashSet};
use std::ops::{Add, Mul, Sub};

use num_integer::Integer;
use num_traits::sign::Signed;
use num_traits::{One, Zero};

use crate::presburger::{ComparisonOp, Formula, Name, Term, Z};

/// A quantifier-free negation normal form Presburger formula.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NnfFormula {
    True,
    False,
    LessThanZero(LinTerm),
    Divisible(Z, LinTerm),
    NotDivisible(Z, LinTerm),
    And(Box<NnfFormula>, Box<NnfFormula>),
    Or(Box<NnfFormula>, Box<NnfFormula>),
    BigAnd(Name, Z, Box<NnfFormula>),
    BigOr(Name, Z, Box<NnfFormula>),
}

/// A linear form of Presburger term.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LinTerm {
    constant: Z,
    coefficients: BTreeMap<Name, Z>,
}

impl NnfFormula {
    fn less_than_zero(t: LinTerm) -> NnfFormula {
        if t.is_constant() {
            if t.constant.is_negative() {
                NnfFormula::True
            } else {
                NnfFormula::False
            }
        } else {
            NnfFormula::LessThanZero(t)
        }
    }

    fn divisible<K: Into<Z>>(k: K, t: LinTerm) -> NnfFormula {
        let k = k.into();

        if k.abs().is_one() {
            return NnfFormula::True;
        }

        if k.is_zero() {
            // t == 0 iff t - 1 < 0 and -t - 1 < 0
            return NnfFormula::and(
                NnfFormula::less_than_zero(&t - 1),
                NnfFormula::less_than_zero(-1 * t - 1),
            );
        }

        NnfFormula::Divisible(k, t)
    }

    fn not_divisible<K: Into<Z>>(k: K, t: LinTerm) -> NnfFormula {
        let k = k.into();

        if k.abs().is_one() {
            return NnfFormula::False;
        }

        if k.is_zero() {
            // t != 0 iff t < 0 or -t < 0
            return NnfFormula::or(
                NnfFormula::less_than_zero(t.clone()),
                NnfFormula::less_than_zero(-1 * t),
            );
        }

        NnfFormula::NotDivisible(k, t)
    }

    fn and(l: NnfFormula, r: NnfFormula) -> NnfFormula {
        match (l, r) {
            (NnfFormula::False, _) | (_, NnfFormula::False) => NnfFormula::False,
            (NnfFormula::True, f) | (f, NnfFormula::True) => f,
            (l, r) => NnfFormula::And(Box::new(l), Box::new(r)),
        }
    }

    fn or(l: NnfFormula, r: NnfFormula) -> NnfFormula {
        match (l, r) {
            (NnfFormula::True, _) | (_, NnfFormula::True) => NnfFormula::True,
            (NnfFormula::False, f) | (f, NnfFormula::False) => f,
            (l, r) => NnfFormula::Or(Box::new(l), Box::new(r)),
        }
    }

    fn big_and<S: Into<Name>, I: Into<Z>>(x: S, max: I, f: NnfFormula) -> NnfFormula {
        let name = x.into();
        if !f.contains_name(&name) {
            return f;
        }
        NnfFormula::BigAnd(name, max.into(), Box::new(f))
    }

    fn big_or<S: Into<Name>, I: Into<Z>>(x: S, max: I, f: NnfFormula) -> NnfFormula {
        let name = x.into();
        if !f.contains_name(&name) {
            return f;
        }
        NnfFormula::BigOr(name, max.into(), Box::new(f))
    }

    fn contains_name(&self, name: &Name) -> bool {
        match self {
            NnfFormula::True | NnfFormula::False => false,
            NnfFormula::LessThanZero(t)
            | NnfFormula::Divisible(_, t)
            | NnfFormula::NotDivisible(_, t) => t.coefficients.contains_key(name),
            NnfFormula::And(l, r) | NnfFormula::Or(l, r) => {
                l.contains_name(name) || r.contains_name(name)
            }
            NnfFormula::BigAnd(x, _, f) | NnfFormula::BigOr(x, _, f) => {
                x != name && f.contains_name(name)
            }
        }
    }

    fn negate(self) -> NnfFormula {
        match self {
            NnfFormula::True => NnfFormula::False,
            NnfFormula::False => NnfFormula::True,
            NnfFormula::LessThanZero(t) => NnfFormula::less_than_zero(-1 * t - 1),
            NnfFormula::Divisible(k, t) => NnfFormula::not_divisible(k, t),
            NnfFormula::NotDivisible(k, t) => NnfFormula::divisible(k, t),
            NnfFormula::And(l, r) => NnfFormula::or(l.negate(), r.negate()),
            NnfFormula::Or(l, r) => NnfFormula::and(l.negate(), r.negate()),
            NnfFormula::BigAnd(name, max, t) => NnfFormula::big_or(name, max, t.negate()),
            NnfFormula::BigOr(name, max, t) => NnfFormula::big_and(name, max, t.negate()),
        }
    }

    fn coefficients(&self, name: &Name) -> HashSet<Z> {
        match self {
            NnfFormula::True | NnfFormula::False => HashSet::new(),
            NnfFormula::LessThanZero(t)
            | NnfFormula::Divisible(_, t)
            | NnfFormula::NotDivisible(_, t) => t
                .coefficients
                .get(name)
                .into_iter()
                .map(|k| k.clone())
                .collect(),
            NnfFormula::And(l, r) | NnfFormula::Or(l, r) => {
                let mut ks = l.coefficients(name);
                ks.extend(r.coefficients(name));
                ks
            }
            NnfFormula::BigAnd(x, _, f) | NnfFormula::BigOr(x, _, f) => {
                if name == x {
                    return HashSet::new();
                }
                f.coefficients(name)
            }
        }
    }

    fn normalize(self, name: &Name, lcm: &Z) -> NnfFormula {
        match self {
            NnfFormula::True => NnfFormula::True,
            NnfFormula::False => NnfFormula::False,
            NnfFormula::LessThanZero(t) => NnfFormula::less_than_zero(t.normalize(name, lcm).1),
            NnfFormula::Divisible(k, t) => {
                let (n, t) = t.normalize(name, lcm);
                NnfFormula::divisible(k * n, t)
            }
            NnfFormula::NotDivisible(k, t) => {
                let (n, t) = t.normalize(name, lcm);
                NnfFormula::not_divisible(k * n, t)
            }
            NnfFormula::And(l, r) => {
                NnfFormula::and(l.normalize(name, lcm), r.normalize(name, lcm))
            }
            NnfFormula::Or(l, r) => NnfFormula::or(l.normalize(name, lcm), r.normalize(name, lcm)),
            NnfFormula::BigAnd(x, max, t) => {
                if &x == name {
                    return NnfFormula::BigAnd(x, max, t);
                }
                NnfFormula::big_and(x, max, t.normalize(name, lcm))
            }
            NnfFormula::BigOr(x, max, f) => {
                if &x == name {
                    return NnfFormula::BigOr(x, max, f);
                }
                NnfFormula::big_or(x, max, f.normalize(name, lcm))
            }
        }
    }

    fn divs(&self, name: &Name) -> HashSet<Z> {
        match self {
            NnfFormula::True | NnfFormula::False | NnfFormula::LessThanZero(_) => HashSet::new(),
            NnfFormula::Divisible(k, t) | NnfFormula::NotDivisible(k, t) => {
                if !t.coefficients.contains_key(name) {
                    return HashSet::new();
                }
                HashSet::from([k.clone()])
            }
            NnfFormula::And(l, r) | NnfFormula::Or(l, r) => {
                let mut ds = l.divs(name);
                ds.extend(r.divs(name));
                ds
            }
            NnfFormula::BigAnd(x, _, f) | NnfFormula::BigOr(x, _, f) => {
                if x == name {
                    return HashSet::new();
                }
                f.divs(name)
            }
        }
    }

    fn assign_min_inf(self, name: &Name) -> NnfFormula {
        match self {
            NnfFormula::True => NnfFormula::True,
            NnfFormula::False => NnfFormula::False,
            NnfFormula::LessThanZero(t) => match t.coefficients.get(name) {
                Some(k) if k.is_positive() => NnfFormula::True,
                Some(k) if k.is_negative() => NnfFormula::False,
                _ => NnfFormula::LessThanZero(t),
            },
            NnfFormula::Divisible(k, t) => NnfFormula::Divisible(k, t),
            NnfFormula::NotDivisible(k, t) => NnfFormula::NotDivisible(k, t),
            NnfFormula::And(l, r) => {
                NnfFormula::and(l.assign_min_inf(name), r.assign_min_inf(name))
            }
            NnfFormula::Or(l, r) => NnfFormula::or(l.assign_min_inf(name), r.assign_min_inf(name)),
            NnfFormula::BigAnd(x, max, f) => {
                if &x == name {
                    return NnfFormula::BigAnd(x, max, f);
                }
                NnfFormula::big_and(x, max, f.assign_min_inf(name))
            }
            NnfFormula::BigOr(x, max, f) => {
                if &x == name {
                    return NnfFormula::BigOr(x, max, f);
                }
                NnfFormula::big_or(x, max, f.assign_min_inf(name))
            }
        }
    }

    fn bounds(&self, name: &Name) -> HashSet<LinTerm> {
        match self {
            NnfFormula::True
            | NnfFormula::False
            | NnfFormula::Divisible(_, _)
            | NnfFormula::NotDivisible(_, _) => HashSet::new(),
            NnfFormula::LessThanZero(t) => match t.coefficients.get(name) {
                Some(k) if k.is_negative() => HashSet::from([t.clone().removed(name)]),
                _ => HashSet::new(),
            },
            NnfFormula::And(l, r) | NnfFormula::Or(l, r) => {
                let mut bs = l.bounds(name);
                bs.extend(r.bounds(name));
                bs
            }
            NnfFormula::BigAnd(x, _, f) | NnfFormula::BigOr(x, _, f) => {
                if x == name {
                    return HashSet::new();
                }
                f.bounds(name)
            }
        }
    }

    fn assign(self, name: &Name, term: &LinTerm) -> NnfFormula {
        match self {
            NnfFormula::True => NnfFormula::True,
            NnfFormula::False => NnfFormula::False,
            NnfFormula::LessThanZero(t) => NnfFormula::less_than_zero(t.assign(name, term)),
            NnfFormula::Divisible(k, t) => NnfFormula::divisible(k, t.assign(name, term)),
            NnfFormula::NotDivisible(k, t) => NnfFormula::not_divisible(k, t.assign(name, term)),
            NnfFormula::And(l, r) => NnfFormula::and(l.assign(name, term), r.assign(name, term)),
            NnfFormula::Or(l, r) => NnfFormula::or(l.assign(name, term), r.assign(name, term)),
            NnfFormula::BigAnd(x, max, f) => {
                if &x == name {
                    return NnfFormula::BigAnd(x, max, f);
                }
                NnfFormula::big_and(x, max, f.assign(name, term))
            }
            NnfFormula::BigOr(x, max, f) => {
                if &x == name {
                    return NnfFormula::BigOr(x, max, f);
                }
                NnfFormula::big_or(x, max, f.assign(name, term))
            }
        }
    }

    /// Checks this formula is truthy on the given valuation.
    ///
    /// When the variable value is missing in the valuation, this value is treated as zero.
    ///
    /// ##### Examples
    ///
    /// ```
    /// # use std::collections::BTreeMap;
    /// #
    /// # use repa::presburger::{FormulaParser, Name, Z};
    /// # use repa::solver::NnfFormula;
    /// #
    /// let formula = FormulaParser::new("x + 1 = 2").parse().unwrap();
    /// let nnf_formula = NnfFormula::from(formula);
    ///
    /// assert_eq!(nnf_formula.check(&BTreeMap::from([(Name::from("x"), Z::from(1))])), true);
    /// assert_eq!(nnf_formula.check(&BTreeMap::from([(Name::from("x"), Z::from(-1))])), false);
    ///
    /// assert_eq!(nnf_formula.check(&BTreeMap::new()), false);
    /// ```
    pub fn check(&self, valuation: &BTreeMap<Name, Z>) -> bool {
        match self {
            NnfFormula::True => true,
            NnfFormula::False => false,
            NnfFormula::LessThanZero(t) => t.evaluate(valuation).is_negative(),
            NnfFormula::Divisible(k, t) => t.evaluate(valuation).is_multiple_of(k),
            NnfFormula::NotDivisible(k, t) => !t.evaluate(valuation).is_multiple_of(k),
            NnfFormula::And(l, r) => l.check(valuation) && r.check(valuation),
            NnfFormula::Or(l, r) => l.check(valuation) || r.check(valuation),
            NnfFormula::BigAnd(x, max, f) => {
                let mut vs = valuation.clone();
                let mut i: Z = One::one();
                while &i <= max {
                    vs.insert(x.clone(), i.clone());
                    if !f.check(&vs) {
                        return false;
                    }
                    i += 1;
                }
                return true;
            }
            NnfFormula::BigOr(x, max, f) => {
                let mut vs = valuation.clone();
                let mut i: Z = One::one();
                while &i <= max {
                    vs.insert(x.clone(), i.clone());
                    if f.check(&vs) {
                        return true;
                    }
                    i += 1;
                }
                return false;
            }
        }
    }
}

impl From<Formula> for NnfFormula {
    fn from(formula: Formula) -> NnfFormula {
        match formula {
            Formula::True => NnfFormula::True,
            Formula::False => NnfFormula::False,
            Formula::Comparison(op, lt, rt) => {
                let l = &LinTerm::from(lt);
                let r = &LinTerm::from(rt);
                match op {
                    // l < r iff l - r < 0
                    ComparisonOp::LessThan => NnfFormula::LessThanZero(l - r),
                    // l <= r iff l < r + 1 iff l - r - 1 < 0
                    ComparisonOp::LessThanOrEqual => NnfFormula::LessThanZero(l - r - 1),
                    // l > r iff r < l iff r - l < 0
                    ComparisonOp::GreaterThan => NnfFormula::LessThanZero(r - l),
                    // l >= r iff l + 1 > r iff r < l + 1 iff r - l - 1 < 0
                    ComparisonOp::GreaterThanOrEqual => NnfFormula::LessThanZero(r - l - 1),
                    // l == r iff l < r + 1 and r < l + 1
                    ComparisonOp::Equal => NnfFormula::And(
                        Box::new(NnfFormula::LessThanZero(l - r - 1)),
                        Box::new(NnfFormula::LessThanZero(r - l - 1)),
                    ),
                    // l != r iff l < r or r < l iff l - r < 0 or r - l < 0
                    ComparisonOp::NotEqual => NnfFormula::Or(
                        Box::new(NnfFormula::LessThanZero(l - r)),
                        Box::new(NnfFormula::LessThanZero(r - l)),
                    ),
                }
            }
            Formula::Divisible(k, t) => NnfFormula::Divisible(k, LinTerm::from(t)),
            Formula::And(l, r) => NnfFormula::And(
                Box::new(NnfFormula::from(*l)),
                Box::new(NnfFormula::from(*r)),
            ),
            Formula::Or(l, r) => NnfFormula::Or(
                Box::new(NnfFormula::from(*l)),
                Box::new(NnfFormula::from(*r)),
            ),
            Formula::Not(t) => NnfFormula::from(*t).negate(),
            Formula::ForAll(x, f) => {
                NnfFormula::from(Formula::ForSome(x, Box::new(Formula::Not(f)))).negate()
            }
            Formula::ForSome(x, f0) => {
                fn lcm(ns: HashSet<Z>) -> Z {
                    ns.iter()
                        .map(|n| n.abs())
                        .fold(One::one(), |a, b| &a * &b / a.gcd(&b))
                }

                let f1 = NnfFormula::from(*f0);
                let ks = f1.coefficients(&x);
                if ks.is_empty() {
                    return f1;
                }

                let m = lcm(ks);
                let f2 = NnfFormula::And(
                    Box::new(f1.normalize(&x, &m)),
                    Box::new(NnfFormula::Divisible(m, LinTerm::var(x.clone()))),
                );
                let d = lcm(f2.divs(&x));
                let f31 = f2.clone().assign_min_inf(&x);
                let bs = f2.bounds(&x);
                if bs.is_empty() {
                    return NnfFormula::big_or(x, d, f31);
                }

                let f32 = bs
                    .iter()
                .map(|b| f2.clone().assign(&x, &(b + &LinTerm::var(x.clone()))))
                    .fold(NnfFormula::False, |l, r| NnfFormula::or(l, r));
                NnfFormula::big_or(x, d, NnfFormula::or(f31, f32))
            }
        }
    }
}

impl From<Term> for LinTerm {
    fn from(term: Term) -> LinTerm {
        match term {
            Term::Const(k) => LinTerm::from(k),
            Term::Var(name) => LinTerm::var(name),
            Term::Add(t1, t2) => LinTerm::from(*t1) + LinTerm::from(*t2),
            Term::Sub(t1, t2) => LinTerm::from(*t1) - LinTerm::from(*t2),
            Term::Mul(k, t) => k * LinTerm::from(*t),
        }
    }
}

impl Add<&LinTerm> for &LinTerm {
    type Output = LinTerm;

    fn add(self, rhs: &LinTerm) -> LinTerm {
        let mut coefficients = self.coefficients.clone();
        for (x, c2) in rhs.coefficients.iter() {
            let c1 = coefficients.entry(x.clone()).or_insert(Zero::zero());
            *c1 += c2;
        }
        LinTerm::new(&self.constant + &rhs.constant, coefficients)
    }
}

impl Add<LinTerm> for LinTerm {
    type Output = LinTerm;

    fn add(self, rhs: LinTerm) -> LinTerm {
        &self + &rhs
    }
}

impl Add<i64> for LinTerm {
    type Output = LinTerm;

    fn add(self, rhs: i64) -> LinTerm {
        self + LinTerm::from(rhs)
    }
}

impl Add<i64> for &LinTerm {
    type Output = LinTerm;

    fn add(self, rhs: i64) -> LinTerm {
        self + &LinTerm::from(rhs)
    }
}

impl Sub<&LinTerm> for &LinTerm {
    type Output = LinTerm;

    fn sub(self, rhs: &LinTerm) -> LinTerm {
        let mut coefficients = self.coefficients.clone();
        for (x, c2) in rhs.coefficients.iter() {
            let c1 = coefficients.entry(x.clone()).or_insert(Zero::zero());
            *c1 -= c2;
        }
        LinTerm::new(&self.constant - &rhs.constant, coefficients)
    }
}

impl Sub<LinTerm> for LinTerm {
    type Output = LinTerm;

    fn sub(self, rhs: LinTerm) -> LinTerm {
        &self - &rhs
    }
}

impl Sub<i64> for LinTerm {
    type Output = LinTerm;

    fn sub(self, rhs: i64) -> LinTerm {
        self - LinTerm::from(rhs)
    }
}

impl Sub<i64> for &LinTerm {
    type Output = LinTerm;

    fn sub(self, rhs: i64) -> LinTerm {
        self - &LinTerm::from(rhs)
    }
}

impl Mul<&LinTerm> for &Z {
    type Output = LinTerm;

    fn mul(self, rhs: &LinTerm) -> LinTerm {
        let mut coefficients = rhs.coefficients.clone();
        for (_, c) in coefficients.iter_mut() {
            *c *= self;
        }
        LinTerm::new(self * &rhs.constant, coefficients)
    }
}

impl Mul<LinTerm> for Z {
    type Output = LinTerm;

    fn mul(self, rhs: LinTerm) -> LinTerm {
        &self * &rhs
    }
}

impl Mul<&LinTerm> for i64 {
    type Output = LinTerm;

    fn mul(self, rhs: &LinTerm) -> LinTerm {
        &Z::from(self) * rhs
    }
}

impl Mul<LinTerm> for i64 {
    type Output = LinTerm;

    fn mul(self, rhs: LinTerm) -> LinTerm {
        Z::from(self) * rhs
    }
}

impl From<Z> for LinTerm {
    fn from(k: Z) -> LinTerm {
        LinTerm::new(k, BTreeMap::new())
    }
}

impl From<i64> for LinTerm {
    fn from(k: i64) -> LinTerm {
        LinTerm::from(Z::from(k))
    }
}

impl LinTerm {
    fn new<I: Into<Z>>(constant: I, mut coefficients: BTreeMap<Name, Z>) -> LinTerm {
        coefficients.retain(|_, v| *v != Zero::zero());
        LinTerm {
            constant: constant.into(),
            coefficients: coefficients,
        }
    }

    fn is_constant(&self) -> bool {
        self.coefficients.is_empty()
    }

    fn var<S: Into<Name>>(name: S) -> LinTerm {
        LinTerm::new(0, BTreeMap::from([(name.into(), One::one())]))
    }

    fn normalize(self, name: &Name, lcm: &Z) -> (Z, LinTerm) {
        match self.coefficients.get(name) {
            Some(k) => {
                let n = lcm / k.abs();
                let l = &n * &self.clone().removed(name);
                let r = k.signum() * LinTerm::var(name.clone());
                (n, l + r)
            }
            None => (One::one(), self),
        }
    }

    fn removed(self, name: &Name) -> LinTerm {
        let mut coefficients = self.coefficients.clone();
        coefficients.remove(name);
        LinTerm::new(self.constant, coefficients)
    }

    fn assign(self, name: &Name, term: &LinTerm) -> LinTerm {
        match self.coefficients.get(name) {
            Some(k) => self.clone().removed(name) + k * term,
            None => self,
        }
    }

    fn evaluate(&self, valuation: &BTreeMap<Name, Z>) -> Z {
        let mut sum = self.constant.clone();
        for (x, k) in self.coefficients.iter() {
            sum += k * valuation.get(x).unwrap_or(&Zero::zero());
        }
        sum
    }
}

#[cfg(test)]
fn parse(source: &str) -> Formula {
    use crate::presburger::FormulaParser;
    FormulaParser::new(source).parse().unwrap()
}

#[test]
fn test_nnf_formula() {
    // From http://www2.imm.dtu.dk/courses/02917/Presburger1.pdf.
    let f = parse("for some x. (3x + 1 < 10 or 7x - 6 > 7) and 2 | x");
    let nnf = NnfFormula::from(f);
    assert_eq!(
        nnf,
        NnfFormula::big_or(
            "x",
            42,
            NnfFormula::or(
                NnfFormula::and(
                    NnfFormula::divisible(42, LinTerm::var("x")),
                    NnfFormula::divisible(21, LinTerm::var("x")),
                ),
                NnfFormula::and(
                    NnfFormula::and(
                        NnfFormula::or(
                            NnfFormula::less_than_zero(LinTerm::var("x") - 24),
                            NnfFormula::less_than_zero(-1 * LinTerm::var("x"))
                        ),
                        NnfFormula::divisible(42, LinTerm::var("x") + 39)
                    ),
                    NnfFormula::divisible(21, LinTerm::var("x") + 39)
                ),
            )
        )
    );
    assert_eq!(nnf.check(&BTreeMap::new()), true);
}
