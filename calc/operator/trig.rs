//! Trigonometry

use super::unbox_it;
use super::super::literal::BigNum;
use super::super::{CalcResult, Environment, Evaluate};
use super::super::common::{str_to_rational, rational_to_f64_trig};

pub fn trig(args: &Vec<Box<Evaluate>>, env: &mut Environment, op: |f64| -> f64 ) -> CalcResult {
    if args.len() > 1 {
        return Err("'sin' takes one argument".to_str())
    }

    let evaluated_array = try!(unbox_it(args, env));
    let evaluated = match evaluated_array.as_slice()[0] {
        BigNum(ref x)   => x,
        _           => return Err("This cannot be evaluated by trigonometric functions".to_str())
    };
        
    let answer = try!(str_to_rational(op(rational_to_f64_trig(evaluated)).to_str().as_slice()));
        
    Ok(BigNum(answer))
}
