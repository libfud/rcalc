//! Trigonometry

use std::num;
use super::super::literal::BigNum;
use super::super::{CalcResult, Environment, Atom, ArgType, BadNumberOfArgs, 
                   BigRational, BadArgType};
use super::super::tokenize::str_to_rational;

static PI: &'static str = "3126535/995207";

pub fn trig(args: &Vec<ArgType>, env: &mut Environment,
            op: |f64| -> f64 ) -> CalcResult {
    if args.len() > 1 {
        return Err(BadNumberOfArgs("'sin' takes one argument".to_str()))
    }

    let evaluated = match try!(args.get(0).arg_to_literal(env)) {
        BigNum(ref x)   => x.clone(),
        _  => return Err(BadArgType("Only numbers can use trigonometric functions".to_str()))
    };
        
    let answer = try!(str_to_rational(op(rational_to_f64_trig(
        &evaluated)).to_str().as_slice()));
        
    Ok(Atom(BigNum(answer)))
}

/// Reduces a rational to a value in f64s range of accuracy, then converts it to an f64
/// and then returns that value for use in trigonometric functions.
pub fn rational_to_f64_trig(bigrational_orig: &BigRational) -> f64 {
    let mut bigrational = bigrational_orig.clone();

    let two: BigRational = num::one::<BigRational>() + num::one();
    let twopi = from_str::<BigRational>(PI).unwrap().mul(&two);
    let jump = from_str::<BigRational>("100000000000/1").unwrap() * twopi;
    let upper = from_str::<BigRational>("9007199254740992/1").unwrap(); //2^53
    let lower = from_str::<BigRational>("-9007199254740992/1").unwrap();

    if bigrational > upper {
        while bigrational > upper {
            bigrational = bigrational - jump;
        }
    } else if bigrational < lower {
        while bigrational < lower {
            bigrational = bigrational + jump;
        }
    }

    let numer = bigrational.numer().to_f64().unwrap();
    let denom = bigrational.denom().to_f64().unwrap();

    numer / denom
}
