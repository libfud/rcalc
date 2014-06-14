//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Evaluate, Environment, Token};

#[deriving(Show, Clone, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
//    Func(String),
    Proc(Vec<String>, Vec<Token>),
    Void
}

#[deriving(Clone)]
pub struct BigNumArg (pub BigRational);

impl Evaluate for BigNumArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let BigNumArg(x) = self.clone(); 
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
pub struct SymbolArg(pub String);

impl Evaluate for SymbolArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let SymbolArg(x) = self.clone();
        Ok(Symbol(x))
    }
}

#[deriving(Clone)]
pub struct ProcArg(pub Vec<String>, pub Vec<Token>);

impl Evaluate for ProcArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let ProcArg(x, y) = self.clone();
        Ok(Proc(x, y))
    }
}

pub struct VoidArg;

impl Evaluate for VoidArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        Ok(Void)
    }
}

/// Translates a literal token into its value
pub fn trans_literal(lit: LiteralType, env: &mut Environment) -> CalcResult<Box<Evaluate>> {
    match lit {
        BigNum(x)   => Ok(box BigNumArg(x) as Box<Evaluate>),
        Boolean(x)  => Ok(box BoolArg(x) as Box<Evaluate>),
        Proc(x, y)  => Ok(box ProcArg(x, y) as Box<Evaluate>),
        Symbol(x)   => trans_literal(try!(env.lookup(&x)), env),
        Void        => Ok(box VoidArg as Box<Evaluate>)
    }
}
