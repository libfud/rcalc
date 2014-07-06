//! An enumeration of valid literaltypes

extern crate num;
extern crate matrix;

use self::matrix::{Matrice, FakeNum};
use super::{BigRational, CalcResult, BadArgType, DivByZero, Expression, ErrorKind, Environment};
use std::num;
use std::fmt;

#[deriving(Clone, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    List(Vec<LiteralType>),
    Matrix(Matrice<LiteralType>),
    Proc(Vec<String>, Expression),
    Symbol(String),
    Void
}

pub struct WithEnv<'a> {
    env: &'a Environment,
    data: &'a LiteralType
}

impl<'a> fmt::Show for WithEnv<'a> {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            &Boolean(ref x) => print!("{}", x),
            &BigNum(ref x) => if x.is_integer() {
                print!("{}", x.numer())
            } else {
                print!("{}", x)
            },
            &List(ref list) => print!("{}", list),
            &Matrix(ref m) => print!("{}", m),
            &Proc(ref args, ref expr) => {
                print!("Procedure: parameters: {}, body: {}", args, expr)
            },
            &Symbol(ref s) => print!("{} {}", s, match self.env.lookup(s) {
                Ok(x) => format!("= {}", x),
                Err(m) => m.to_str(),
            }),
            &Void => print!("")
        }

        Ok(())
    }
}

impl fmt::Show for LiteralType {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Boolean(ref x) => print!("{}", x),
            &BigNum(ref x) => if x.is_integer() {
                print!("{}", x.numer())
            } else {
                print!("{}", x)
            },
            &List(ref list) => print!("{}", list),
            &Matrix(ref m) => print!("{}", m),
            &Proc(ref args, ref expr) => {
                print!("Procedure: parameters: {}, body: {}", args, expr)
            },
            &Symbol(ref s) => print!("{}", s),
            &Void => print!("")
        }

        Ok(())
    }
}

pub type Lit = LiteralType;
pub type LitRes =  CalcResult<LiteralType>;
type BR = BigRational;

fn apply(a: &Lit, b: &Lit, op: |&BR, &BR| -> BR) -> LitRes {
    match (a, b) {
        (&BigNum(ref x), &BigNum(ref y)) => Ok(BigNum(op(x, y))),
        _ => Err(BadArgType("Arithmetic is only defined for numbers".to_str()))
    }
}

fn apply_div(a: &Lit, b: &Lit, op: |&BR, &BR| -> BR) -> LitRes {
    match (a, b) {
        (&BigNum(ref x), &BigNum(ref y)) => if y == &num::zero() {
            Err(DivByZero)
        } else {
            Ok(BigNum(op(x, y)))
        },
        _ => Err(BadArgType("Division is only defined for numbers".to_str()))
    }
}

impl FakeNum<Lit, ErrorKind> for Lit { }

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

