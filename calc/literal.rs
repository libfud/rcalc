//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Evaluate, Environment, Token};

#[deriving(Clone)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, Box<Evaluate>),
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
    fn eval(&self, env: &mut Environment) -> CalcResult {
        let arg = match self {
            &SymbolArg(ref x) => x.clone(),
        };
        env.lookup(&arg)
    }
}

pub struct ProcArg(pub Vec<String>, pub Box<Evaluate>);

impl Evaluate for ProcArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let &ProcArg(ref x, ref y) = self;
        Ok(Proc(x.clone(), *y))
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
