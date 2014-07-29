//! Trigonometry

extern crate num;
extern crate types;

use self::num::rational::Ratio;
use self::types::operator::*;
use self::types::literal::BigNum;
use self::types::sexpr::{Atom, ArgType};
use super::super::{CalcResult, Environment, Evaluate, BadNumberOfArgs, BadFloatRange};

pub fn float_ops(args: &Vec<ArgType>, env: &mut Environment, fop: Transcendental) -> CalcResult {
    if args.len() > 1 {
        return Err(BadNumberOfArgs(fop.to_string(), "only".to_string(), 1))
    }

    let floated = try!(try!(args[0].desymbolize(env)).to_f64());

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
