//! Methods of raising an index to a given power.

extern crate types;

use std::num;
use super::super::{BigNum, CalcResult, Environment, Evaluate, BadArgType,
                   BadNumberOfArgs, BadPowerRange};
use super::{BigRational, ArgType, Atom};
use super::trig::float_ops;
use self::types::operator::{Ln, Exp};

pub fn pow_wrapper(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("pow".to_string(), "only".to_string(), 2))
    } 

    let (base, exponent) = match (try!(args[0].desymbolize(env)),
                                  try!(args[1].desymbolize(env))) {
        (BigNum(x), BigNum(y)) => (x, y),
        _ => return Err(BadArgType("Only numbers can be raised to a power".to_string()))
    };

    Ok(Atom(BigNum(try!(pow(&base, &exponent, env)))))
}

pub fn pow(base: &BigRational, exponent: &BigRational, 
           env: &mut Environment) -> CalcResult<BigRational> {

    let power = match exponent.to_integer().to_u64() {
        Some(x) => x,
        None    => return Err(BadPowerRange)
    };

    let maybe_radix = exponent - exponent.floor();
    let root  = if maybe_radix == num::zero() {
        num::one()
    } else {
        let root_log = match try!(float_ops(&vec!(Atom(BigNum(base.clone()))), env, Ln)) {
            Atom(BigNum(x)) => x,
            _ => fail!("impossible")
        };
        let r_log_mul = root_log * maybe_radix;
        match try!(float_ops(&vec!(Atom(BigNum(r_log_mul))), env, Exp)) {
            Atom(BigNum(x)) => x,
            _ => fail!("Impossible")
        }
    };

    let powered = exp_by_sq(base, power);

    Ok(powered * root)
}

pub fn exp_by_sq(base_orig: &BigRational, mut power: u64) -> BigRational {
    let mut base = base_orig.clone();
    let mut result: BigRational = num::one();

    while power > 0 {
        if power % 2 == 1 {
            result = result * base;
        }
        power /= 2;
        base = base * base;
    }

    result
}

  
