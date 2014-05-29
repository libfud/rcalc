//! implementation of the eval() method for Number (BigRational)
//! used by translate for boxing individual numbers

extern crate num;

use super::{CalcResult, Evaluate, Environment};
use super::literal::{BigNum, Boolean, Matrix, Symbol, Func};
use self::num::rational::BigRational;

#[deriving(Clone, Ord, Eq)]
pub struct Number(pub BigRational);

impl Evaluate for Number {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let Number(x) = self.clone(); 
        Ok(BigNum(x))
    }
}

pub struct BoolArg(pub bool);

impl Evaluate for BoolArg {
    fn eval(&self, _: &mut Environment)  -> CalcResult {
        let &BoolArg(x) = self;
        Ok(Boolean(x))
    }
}

#[deriving(Clone)]
pub struct MatrixArg(pub Vec<BigRational>);

impl Evaluate for MatrixArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let MatrixArg(x)   = self.clone();
        Ok(Matrix(x))
    } 
}

#[deriving(Clone)]
pub struct SymbolArg(pub String);

impl Evaluate for SymbolArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let SymbolArg(x) = self.clone();
        Ok(Symbol(x))
    }
}

#[deriving(Clone)]
pub struct FunArg(pub String);

impl Evaluate for FunArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let FunArg(x) = self.clone();
        Ok(Func(x))
    }
}
