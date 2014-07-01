//! Trigonometry

use super::*;
use super::super::literal::BigNum;
use super::super::{CalcResult, Environment, Atom, ArgType, BadNumberOfArgs, 
                   BadArgType, BadFloatRange};
use super::super::tokenize::str_to_rational;

pub fn float_ops(args: &Vec<ArgType>, env: &mut Environment, fop: OperatorType) -> CalcResult {
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
        _ => fail!("That shouldn't happen")
    };
        
    let result = try!(str_to_rational(answer.to_str().as_slice()));
        
    Ok(Atom(BigNum(result)))
}

pub fn rational_to_f64(big: &Mpq) -> CalcResult<f64> {
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
