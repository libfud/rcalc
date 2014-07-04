//! An enumeration of valid literaltypes

use super::{CalcResult, Environment, Atom, BadArgType, BadNumberOfArgs};
use super::type::literal::{List};
use super::matrix::Matrice;
use super::expression::{Expression, ArgType};
use std::num;

pub fn list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut list: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        list.push(try!(arg.arg_to_literal(env)));
    }
    Ok(Atom(List(list)))
}

pub fn cons(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `cons'".to_str()))
    }

    let car = try!(args.get(0).arg_to_literal(env));
    let cdr = try!(args.get(1).arg_to_literal(env));

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
