//! The parent module to every other module in calc.

extern crate num;

use self::literal::LiteralType;
use self::tokenize::tokenize;
use self::translate::translate;
pub use self::number::{Number, BoolArg, MatrixArg};
pub use self::common::help;

mod literal;
mod tokenize;
mod translate;
mod expression;
mod number;
mod constant;
mod operator;
mod function;
pub mod common;

// A shortcut for the result type that is used everywhere
pub type CalcResult<T = LiteralType> = Result<T, String>;
pub trait Evaluate {
    fn eval(&self) -> CalcResult;
}

//Evaluates a string
pub fn eval(s: &str) -> CalcResult {
    let tokens = try!(tokenize(s.trim()));
    let expr = try!(translate(tokens.as_slice()));
    expr.eval()
}
