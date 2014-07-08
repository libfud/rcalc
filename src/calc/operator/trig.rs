//! Trigonometry

extern crate num;
extern crate types;

use self::num::rational::Ratio;
use self::types::operator::{Sin, Cos, Tan, ASin, ACos, ATan, SinH, CosH, TanH,
                            ASinH, ACosH, ATanH, Log, Ln, Exp, Transcendental};
use self::types::literal::BigNum;
use self::types::sexpr::{Atom, ArgType};
use super::super::{CalcResult, Environment, Evaluate, BadNumberOfArgs,
                   BigRational, BadArgType, BadFloatRange};

pub fn float_ops(args: &Vec<ArgType>, env: &mut Environment, fop: Transcendental) -> CalcResult {
    if args.len() > 1 {
        return Err(BadNumberOfArgs(format!("{} takes one argument", fop)))
    }

    let floated = match try!(args.get(0).desymbolize(env)) {
        BigNum(ref x)   => try!(rational_to_f64(x)),
        _  => return Err(BadArgType("Only numbers can use trigonometric functions".to_str()))
    };

    let answer = match fop {
        Sin => floated.sin(),
        Cos => floated.cos(),
        Tan => floated.tan(),
        ASin => floated.asin(),
        ACos =>  floated.acos(),
        ATan => floated.atan(),
        SinH => floated.sinh(),
        CosH => floated.cosh(),
        TanH => floated.tanh(),
        ASinH => floated.asinh(),
        ACosH => floated.acosh(),
        ATanH => floated.atanh(),
        Log => floated.log10(),
        Ln => floated.ln(),
        Exp => floated.exp(),
    };
        
    match Ratio::from_float(answer) {
        Some(x) => Ok(Atom(BigNum(x))),
        None => Err(BadFloatRange)
    }
}

pub fn rational_to_f64(big: &BigRational) -> CalcResult<f64> {
    let numer = match big.numer().to_f64() {
        Some(x) => x,
        None => return Err(BadFloatRange)
    };

    let denom = match big.denom().to_f64() {
        Some(x) => x,
        None => return Err(BadFloatRange)
    };

    Ok(numer / denom)
}
