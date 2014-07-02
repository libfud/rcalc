//! An enumeration of valid literaltypes

use super::{BigRational, CalcResult, Environment, Atom, BadArgType, DivByZero, BadNumberOfArgs};
use super::expression::{Expression, ArgType};
use std::num;

#[deriving(Clone, Show, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, Expression),
    List(Vec<LiteralType>),
    Void
}

pub type Lit<T = LiteralType> = T;
pub type LitRes<T = CalcResult<LiteralType>> = T;

impl Add<Lit, LitRes> for Lit {
    fn add(&self, rhs: &LiteralType) -> LitRes {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => Ok(BigNum(*x + *y)),
            _ => Err(BadArgType("Addition is only defined for numbers".to_str()))
        }
    }
}

impl Sub<Lit, LitRes> for Lit {
    fn sub(&self, rhs: &Lit) -> LitRes {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => Ok(BigNum(*x - *y)),
            _ => Err(BadArgType("Subtraction is only defined for numbers".to_str()))
        }
    }
}

impl Mul<Lit, LitRes> for Lit {
    fn mul(&self, rhs: &Lit) -> LitRes {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => Ok(BigNum(*x * *y)),
            _ => Err(BadArgType("Multiplication is only defined for numbers".to_str()))
        }
    }
}

impl Div<Lit, LitRes> for Lit {
    fn div(&self, rhs: &Lit) -> LitRes {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => if y == &num::zero() {
                Err(DivByZero)
            } else {
                Ok(BigNum(*x / *y))
            },
            _ => Err(BadArgType("Division is only defined for numbers".to_str()))
        }
    }
}

impl Rem<Lit, LitRes> for Lit {
    fn rem(&self, rhs: &Lit) -> LitRes {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => if y == &num::zero() {
                Err(DivByZero)
            } else {
                Ok(BigNum(*x % *y))
            },
            _ => Err(BadArgType("Rem is only defined for numbers".to_str()))
        }
    }
}

pub fn list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut list: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        list.push(try!(arg.desymbolize(env)));
    }
    Ok(Atom(List(list)))
}

pub fn cons(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `cons'".to_str()))
    }

    let car = try!(args.get(0).desymbolize(env));
    let cdr = try!(args.get(1).desymbolize(env));

    match cdr {
        List(x) => Ok(Atom(List(vec!(car).append(x.as_slice())))),
        _ => Ok(Atom(List(vec!(car, cdr))))
    }
}

pub fn car(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `car'".to_str()))
    }

    match try!(args.get(0).desymbolize(env)) {
        List(x) => {
            if x.len() < 1 {
                Err(BadArgType("Empty list!".to_str()))
            } else {
                Ok(Atom(x.get(0).clone()))
            }
        },
        _ => Err(BadArgType("Wrong type for `car'".to_str()))
    }
}

pub fn cdr(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `cdr'".to_str()))
    }

    match try!(args.get(0).desymbolize(env)) {
        List(x) => match x.len() {
            0 => Err(BadArgType("List too short!".to_str())),
            _ => Ok(Atom(List(x.tail().to_owned())))
        },
        _ => Err(BadArgType("Wrong type for `cdr'".to_str()))
    }
}
