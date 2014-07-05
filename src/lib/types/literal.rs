//! An enumeration of valid literaltypes

use super::{BigRational, CalcResult, BadArgType, DivByZero, Expression, Matrice};
use std::num;

#[deriving(Clone, Show, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    List(Vec<LiteralType>),
    Matrix(Matrice<LiteralType>),
    Proc(Vec<String>, Expression),
    Symbol(String),
    Void
}

pub type Lit = LiteralType;
pub type LitRes =  CalcResult<LiteralType>;
type BR = BigRational;

fn apply(a: &Lit, b: &Lit, op: |&BigRational, &BigRational| -> BigRational) -> LitRes {
    match (a, b) {
        (&BigNum(ref x), &BigNum(ref y)) => Ok(BigNum(op(x, y))),
        _ => Err(BadArgType("Arithmetic is only defined for numbers".to_str()))
    }
}

fn apply_div(a: &Lit, b: &Lit, op: |&BigRational, &BigRational| -> BigRational) -> LitRes {
    match (a, b) {
        (&BigNum(ref x), &BigNum(ref y)) => if y == &num::zero() {
            Err(DivByZero)
        } else {
            Ok(BigNum(op(x, y)))
        },
        _ => Err(BadArgType("Division is only defined for numbers".to_str()))
    }
}

impl Add<Lit, LitRes> for Lit {
    fn add(&self, rhs: &LiteralType) -> LitRes {
        apply(self, rhs, |a, b| a + *b)
    }
}

impl Sub<Lit, LitRes> for Lit {
    fn sub(&self, rhs: &Lit) -> LitRes {
        apply(self, rhs, |a, b| a - *b)
    }
}

impl Mul<Lit, LitRes> for Lit {
    fn mul(&self, rhs: &Lit) -> LitRes {
        apply(self, rhs, |a, b| a * *b)
    }
}

impl Div<Lit, LitRes> for Lit {
    fn div(&self, rhs: &Lit) -> LitRes {
        apply_div(self, rhs, |a, b| a / *b)
    }
}

impl Rem<Lit, LitRes> for Lit {
    fn rem(&self, rhs: &Lit) -> LitRes {
        apply_div(self, rhs, |a, b| a % *b)
    }
}

