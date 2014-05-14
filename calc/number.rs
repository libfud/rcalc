//! Filler

extern crate num;

use super::{CalcResult, Evaluate};
use self::num::rational::BigRational;

pub struct Number(pub BigRational);

impl Evaluate for Number {
    fn eval(&self) -> CalcResult {
        let n = self.clone();
        let &Number(x) = n;
        Ok(x)
    }
}
