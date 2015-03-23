//! Trigonometry

extern crate num;
extern crate types;

use self::num::rational::Ratio;
use self::types::operator::*;
use self::types::operator::Transcendental::*;
use self::types::literal::LiteralType::BigNum;
use self::types::sexpr::ArgType::Atom;
use self::types::ErrorKind::{BadNumberOfArgs, BadFloatRange};
use super::super::{CalcResult, Args, Env, Evaluate};

pub fn float_ops(args: &Args, env: &mut Env, fop: Transcendental) -> CalcResult {
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
