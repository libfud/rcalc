//!Logic and odering.

extern crate num;

use self::num::rational::BigRational;
use super::super::{Evaluate, CalcResult, Environment};
use super::super::literal::{Boolean, Symbol, BigNum};
use super::unbox_it;

type Args<T = Vec<Box<Evaluate>>> = T;
type Env<T = Environment> = T;

pub fn cond(args: &Args, env: &mut Env)  -> CalcResult {
    if args.len() != 3 {
        return Err("`if` requires three arguments".to_str())
    }

    let condition = match try!(args.get(0).eval(env)) {
        Boolean(x)  => x,
        Symbol(x)   => {
            match try!(env.lookup(&x)) {
                Boolean(y)  => y,
                _   => return Err("Only boolean expressions can be a condtion!"
                                  .to_str())
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

type BR<T = BigRational> = T;

pub fn ordering(args: &Args, env: &mut Env, comp: |&BR,&BR| -> bool) -> CalcResult {

    if args.len() != 2 {
        return Err("Ordering requires two arguments".to_str())
    }
    let comparands = try!(unbox_it(args, env));
    let (a, b) = match (comparands.get(0), comparands.get(1)) {
        (&BigNum(ref x), &BigNum(ref y)) => (x.clone(), y.clone()),
        _ => return Err("Ordering only takes numbers!".to_str())
    };
    Ok(Boolean(comp(&a, &b)))
}

pub fn equality(args: &Args, env: &mut Env, equal: bool) -> CalcResult {
    if args.len() != 2 {
        return Err("Equality comparisons require two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));

    if equal {
        match (comparands.get(0), comparands.get(1)) {
            (&Boolean(x), &Boolean(y)) => Ok(Boolean(x == y)),
            (&BigNum(ref x), &BigNum(ref y)) => Ok(Boolean(x == y)),
            _ => Err("Mixed types!".to_str())
       }
    } else {
        match (comparands.get(0), comparands.get(1)) {
            (&Boolean(x), &Boolean(y)) => Ok(Boolean(x == y)),
            (&BigNum(ref x), &BigNum(ref y)) => Ok(Boolean(x == y)),
            _ => Err("Mixed types!".to_str())
       }
    }
}       

pub fn and_or(args: &Args, env: &mut Env, short: bool) -> CalcResult {
    let vals = try!(unbox_it(args, env));

    if short == true {
        for val in vals.iter() {
            match *val {
                Boolean(true)   => return Ok(Boolean(short)),
                Boolean(false)  => { },
                _   => return Err("Non boolean conditon!".to_str())
            }
        }
        
        Ok(Boolean(false))
    } else {
        for val in vals.iter() {
            match *val {
                Boolean(true)   => { },
                Boolean(false)  => return Ok(Boolean(short)),
                _   => return Err("Non boolean condition!".to_str())
            }
        }

        Ok(Boolean(true))
    }
}

pub fn not(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err("Not only takes one argument".to_str())
    }

    let val = match try!(unbox_it(args, env)).get(0) {
        &Boolean(x)  => x,
        _   => return Err("Non boolean condition!".to_str())
    };

    Ok(Boolean(!val))
}
