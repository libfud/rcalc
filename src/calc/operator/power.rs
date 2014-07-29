//! Methods of raising an index to a given power.

extern crate types;

use std::num;
use super::super::{Atom, ArgType, CalcResult, Environment, Evaluate, BadNumberOfArgs};
use super::trig::float_ops;
use self::types::operator::{Ln, Exp};
use self::types::literal::Lit;

pub fn pow_wrapper(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("pow".to_string(), "only".to_string(), 2))
    } 

    let (base, exponent) = (try!(args[0].desymbolize(env)), try!(args[1].desymbolize(env)));

    Ok(Atom(try!(pow(&base, &exponent, env))))
}

pub fn pow(base: &Lit, exponent: &Lit, env: &mut Environment) -> CalcResult<Lit> {
    let one: Lit = num::one();

    if *base == one || *exponent == one {
        return Ok(base.clone())
    } else if *exponent == num::zero() {
        return Ok(one)
    }

    let positive = *exponent > num::zero();
    let power = try!(exponent.abs().to_uint()) as u64;

    let maybe_radix = exponent - try!(exponent.floor());
    let root  = if maybe_radix == num::zero() {
        num::one()
    } else {
        let root_log = try!(try!(float_ops(&vec!(Atom(base.clone())), env, Ln)).desymbolize(env));
        let r_log_mul = root_log * maybe_radix;
        try!(try!(float_ops(&vec!(Atom(r_log_mul)), env, Exp)).desymbolize(env))
    };

    let powered = exp_by_sq(base, power);

    if positive {
        Ok(powered * root)
    } else { 
        Ok(try!(powered.recip()) * root)
    }
}

pub fn exp_by_sq(base_orig: &Lit, mut power: u64) -> Lit {
    let mut base = base_orig.clone();
    let mut result: Lit = num::one();

    while power > 0 {
        if power % 2 == 1 {
            result = result * base;
        }
        power /= 2;
        base = base * base;
    }

    result
}

  
