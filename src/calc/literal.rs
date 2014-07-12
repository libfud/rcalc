//! An enumeration of valid literaltypes

extern crate types;
extern crate matrix;

use self::types::literal::{LiteralType, List};
use self::types::sexpr::{ArgType, Atom};
use self::types::{BadArgType, BadNumberOfArgs};
use super::{CalcResult, Environment, Evaluate};

pub fn list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut list: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        list.push(try!(arg.arg_to_literal(env)));
    }
    Ok(Atom(List(list)))
}

pub fn cons(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `cons'".to_string()))
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
        return Err(BadNumberOfArgs("Wrong number of arguments to `car'".to_string()))
    }

    match try!(args.get(0).desymbolize(env)) {
        List(x) => {
            if x.len() < 1 {
                Err(BadArgType("Empty list!".to_string()))
            } else {
                Ok(Atom(x.get(0).clone()))
            }
        },
        _ => Err(BadArgType("Wrong type for `car'".to_string()))
    }
}

pub fn cdr(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Wrong number of arguments to `cdr'".to_string()))
    }

    match try!(args.get(0).desymbolize(env)) {
        List(x) => match x.len() {
            0 => Err(BadArgType("List too short!".to_string())),
            _ => Ok(Atom(List(x.tail().to_owned())))
        },
        _ => Err(BadArgType("Wrong type for `cdr'".to_string()))
    }
}
