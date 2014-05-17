//! implementation of the eval() method for Number (BigRational)
//! used by translate for boxing individual numbers

extern crate num;

use super::{CalcResult, Evaluate};
use super::literal::{BigNum, Boolean, Matrix};
use self::num::rational::BigRational;

#[deriving(Clone)]
#[deriving(Ord)]
#[deriving(Eq)]
pub struct Number(pub BigRational);

impl Evaluate for Number {
    fn eval(&self) -> CalcResult {
        let Number(x) = self.clone(); 
        Ok(BigNum(x))
    }
}

pub struct BoolArg(pub bool);

impl Evaluate for BoolArg {
    fn eval(&self)  -> CalcResult {
        let &BoolArg(x) = self;
        Ok(Boolean(x))
    }
}

#[deriving(Clone)]
pub struct MatrixArg(pub Vec<BigRational>);

impl Evaluate for MatrixArg {
    fn eval(&self) -> CalcResult {
        let MatrixArg(x)   = self.clone();
        Ok(Matrix(x))
    } 
}
