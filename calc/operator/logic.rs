//!Logic and odering.

use super::super::{Evaluate, CalcResult, Environment, lookup};
use super::super::literal::{LiteralType, Boolean, Symbol};
use super::unbox_it;

pub fn cond(args: &Vec<Box<Evaluate>>, env: &mut Environment)  -> CalcResult {
    if args.len() != 3 {
        return Err("`if` requires three arguments".to_str())
    }

    let condition = match try!(args.get(0).eval(env)) {
        Boolean(x)  => x,
        Symbol(x)   => {
            match try!(lookup(&x, env)) {
                Boolean(y)  => y,
                _   => return Err("Only boolean expressions can be a condtion!".to_str())
            }
        },
        _   => return Err("Only boolean expressions can be a condition!".to_str())
    };

    if condition {
        Ok(try!(args.get(1).eval(env)))
    } else {
        Ok(try!(args.get(2).eval(env)))
    }
}

pub fn ordering(args: &Vec<Box<Evaluate>>, env: &mut Environment, 
                        comp: |&LiteralType, &LiteralType| -> bool) -> CalcResult {

    if args.len() != 2 {
        return Err("Ordering requires two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));
    Ok(Boolean(comp(comparands.get(0), comparands.get(1))))
}

pub fn equality(args: &Vec<Box<Evaluate>>, env: &mut Environment, equal: bool) -> CalcResult {
    if args.len() != 2 {
        return Err("Equality comparisons require two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));

    if equal {
        Ok(Boolean(comparands.get(0) == comparands.get(1)))
    } else {
        Ok(Boolean(comparands.get(0) != comparands.get(1)))
    }
}

pub fn and_or(args: &Vec<Box<Evaluate>>, env: &mut Environment, op: |bool, bool| -> bool)
                                                                                -> CalcResult {

    if args.len() != 2 {
        return Err("And requires two arguments!".to_str())
    }

    let vals = try!(unbox_it(args, env));

    match (vals.get(0), vals.get(1)) {
        (&Boolean(x), &Boolean(y))  => Ok(Boolean(op(x, y))),
        _   => Err("And doesn't work that way!".to_str())
    }
}
