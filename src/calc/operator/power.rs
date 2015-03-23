//! Methods of raising an index to a given power.

extern crate types;
extern crate num;

use super::super::{Args, Lit, CalcResult, Env, Evaluate};
use super::trig::float_ops;
use self::types::operator::Transcendental::{Ln, Exp};
use self::types::ErrorKind::BadNumberOfArgs;
use self::types::sexpr::ArgType::Atom;
use self::types::LiteralType;

pub fn pow_wrapper(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("pow".to_string(), "only".to_string(), 2))
    } 

    let (base, exponent) = (try!(args[0].desymbolize(env)), try!(args[1].desymbolize(env)));
    Ok(Atom(try!(pow(&base, &exponent, env))))
}

pub fn pow(base: &Lit, exponent: &Lit, env: &mut Env) -> CalcResult<Lit> {
    let one: Lit = num::one();

    if *base == one || *exponent == one {
        return Ok(base.clone())
    } else if *exponent == num::zero() {
        return Ok(one)
    }

    let positive = exponent.clone() > num::zero();
    let power = try!(exponent.clone().abs().to_usize()) as u64;

    let maybe_radix = exponent.clone() - try!(exponent.floor());
    let zero: LiteralType = num::zero();
    let root: LiteralType = if maybe_radix == zero {
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
            result = result.clone() * base.clone();
        }
        power /= 2;
        base = base.clone() * base.clone();
    }

    result
}

  
