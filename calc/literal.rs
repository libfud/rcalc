//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Environment, Atom};
use super::expression::{Expression, ArgType, arg_to_literal};

#[deriving(Clone, Show, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, Expression),
    List(Vec<LiteralType>),
    Void
}

pub fn list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut list: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        list.push(try!(arg_to_literal(arg, env)));
    }
    Ok(Atom(List(list)))
}

pub fn cons(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err("Wrong number of arguments to `cons'".to_str())
    }

    let car = try!(arg_to_literal(args.get(0), env));
    let cdr = try!(arg_to_literal(args.get(1), env));

    match cdr {
        List(x) => Ok(Atom(List(vec!(car).append(x.as_slice())))),
        _ => Ok(Atom(List(vec!(car, cdr))))
    }
}

pub fn car(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("Wrong number of arguments to `car'".to_str())
    }

    match try!(arg_to_literal(args.get(0), env)) {
        List(x) => {
            if x.len() < 1 {
                Err("Empty list!".to_str())
            } else {
                Ok(Atom(x.get(0).clone()))
            }
        },
        _ => Err("Wrong type for `car'".to_str())
    }
}

pub fn cdr(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("Wrong number of arguments to `cdr'".to_str())
    }

    match try!(arg_to_literal(args.get(0), env)) {
        List(x) => match x.len() {
            0 => Err("List too short!".to_str()),
            _ => Ok(Atom(List(x.tail().to_owned())))
        },
        _ => Err("Wrong type for `cdr'".to_str())
    }
}

