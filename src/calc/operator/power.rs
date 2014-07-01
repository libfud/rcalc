//! Methods of raising an index to a given power.

use std::num;
use super::super::{CalcResult, Environment, BadArgType, BadNumberOfArgs, BadPowerRange};
use super::super::literal::BigNum;
use super::{Mpq, ArgType, Atom};
use super::logic::floor;

pub fn pow_wrapper(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut args_vec: Vec<Mpq> = Vec::new();
    for arg in args.iter() {
        match try!(arg.desymbolize(env)) {
            BigNum(x)   => args_vec.push(x),
            _ => return Err(BadArgType("Only numbers can be raised to a power".to_str()))
        }
    }

    Ok(Atom(BigNum(try!(pow(args_vec)))))
}

pub fn pow(args: Vec<Mpq>) -> CalcResult<Mpq> {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("Pow takes a base and an exponent.".to_str()))
    }

    let pow_string = floor(args.get(1)).get_num().to_str();
    let power = match from_str::<u64>(pow_string.as_slice()) {
        Some(x) => x,
        None => {
            println!("{}", pow_string);
            return Err(BadPowerRange)
        }
    };

    Ok(exp_by_sq(args.get(0).clone(), power))
}

pub fn exp_by_sq(base: Mpq, power: u64) -> Mpq {
    if power == 0 {
        num::one()
    } else if power == 1 {
        base
    } else if power % 2 == 0 {
        exp_by_sq(base * base, power / 2)
    } else {
        base * exp_by_sq(base * base, (power - 1) / 2)
    }
}

    
    
        
    
