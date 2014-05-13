extern crate num;

use std::str;
use self::num::rational::BigRational;

// A shortcut for the result type that is used everywhere
pub type CalcResult<T = BigRational> = Result<T, str::MaybeOwned<'static>>;

pub trait Evaluate {
    fn eval(&self) -> CalcResult;
}

//Evaluates a string
pub fn eval(s: &str) -> CalcResult {
    let tokens = try!(tokenize(s.trim()));
    let expr = try!(translate(tokens.as_slice()));
    expr.eval()
}
