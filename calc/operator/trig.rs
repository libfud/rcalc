//! Trigonometry

use super::super::literal::BigNum;
use super::super::{CalcResult, Environment, Atom, ArgType, arg_to_literal};
use super::super::common::{str_to_rational, rational_to_f64_trig};

pub fn trig(args: &Vec<ArgType>, env: &mut Environment,
            op: |f64| -> f64 ) -> CalcResult {
    if args.len() > 1 {
        return Err("'sin' takes one argument".to_str())
    }

    let evaluated = match try!(arg_to_literal(args.get(0), env)) {
        BigNum(ref x)   => x.clone(),
        _  => return Err("This cannot be evaluated by trigonometric functions".to_str())
    };
        
    let answer = try!(str_to_rational(op(rational_to_f64_trig(
        &evaluated)).to_str().as_slice()));
        
    Ok(Atom(BigNum(answer)))
}
