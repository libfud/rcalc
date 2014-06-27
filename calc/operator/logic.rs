//!Logic and odering.

use super::super::{CalcResult, Environment, NonBoolean, BadNumberOfArgs, BadArgType};
use super::super::literal::{Boolean, BigNum};
use super::{ArgType, Atom, BigRational};

type Args<T = Vec<ArgType>> = T;
type Env<T = Environment> = T;

pub fn cond(args: &Args, env: &mut Env)  -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`if` requires three arguments".to_str()))
    }

    let condition = match try!(args.get(0).desymbolize(env)) {
        Boolean(x)  => x,
        _ => return Err(NonBoolean)
    };

    if condition {
        Ok(Atom(try!(args.get(1).desymbolize(env))))
    } else {
        Ok(Atom(try!(args.get(2).desymbolize(env))))
    }
}

type BR = BigRational;

pub fn ordering(args: &Args, env: &mut Env, comp: |&BR,&BR| -> bool) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("Ordering requires two arguments".to_str()))
    }
    let (a, b) = (try!(args.get(0).desymbolize(env)),
                  try!(args.get(1).desymbolize(env)));
    match (&a, &b) {
        (&BigNum(ref x), &BigNum(ref y)) => Ok(Atom(Boolean(comp(x, y)))),
        _ =>  Err(BadArgType(format!("Ordering only takes numbers! {} {}",
                          a, b)))
    }
}

pub fn and_or(args: &Args, env: &mut Env, short: bool) -> CalcResult {

    if short == true {
        for val in args.iter() {
            match try!(val.desymbolize(env)) {
                Boolean(true)   => return Ok(Atom(Boolean(short))), 
                Boolean(false)  => { },
                _   => return Err(NonBoolean)
            }
        }
        
        Ok(Atom(Boolean(false)))
    } else {
        for val in args.iter() {
            match try!(val.desymbolize(env)) {
                Boolean(true)   => { },
                Boolean(false)  => return Ok(Atom(Boolean(short))),
                _   => return Err(NonBoolean)
            }
        }

        Ok(Atom(Boolean(true)))
    }
}

pub fn xor(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("`xor' requires at least two arguments".to_str()))
    }

    let mut result = match try!(args.get(0).desymbolize(env)) {
        Boolean(x) => x,
        _ => return Err(NonBoolean)
    };

    for val in args.tail().iter() {
        result = match try!(val.desymbolize(env)) {
            Boolean(x) => result ^ x,
            _ => return Err(NonBoolean)
        };
    }

    Ok(Atom(Boolean(result)))
}

pub fn not(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Not only takes one argument".to_str()))
    }

    let val = match try!(args.get(0).desymbolize(env)) {
        Boolean(x)  => x,
        _   => return Err(NonBoolean)
    };

    Ok(Atom(Boolean(!val)))
}
