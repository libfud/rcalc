//!Logic and odering.

extern crate num;

use super::super::{Evaluate, CalcResult, Environment, lookup};
use super::super::literal::{Boolean, BigNum, Symbol};
use super::unbox_it;
use self::num::rational::BigRational;

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

//Handles <, <=, >=, >
//== and != can cover booleans themselves
pub fn ordering(args: &Vec<Box<Evaluate>>, env: &mut Environment, 
                        comp: |BigRational, BigRational| -> bool) -> CalcResult {

    if args.len() != 2 {
        return Err("Ordering requires two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));
    match (comparands.get(0), comparands.get(1)) {
        (&BigNum(ref x), &BigNum(ref y))  => Ok(Boolean(comp(x.clone(), y.clone()))),
        _   => Err("Non orderable comparands!".to_str())
    }
}
