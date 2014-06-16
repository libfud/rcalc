//! An enumeration of valid literaltypes

extern crate num;

use std::num;
use self::num::rational::BigRational;
use super::{CalcResult, Evaluate, Environment, Token};

#[deriving(Clone, Show)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
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

    fn to_symbol(&self) -> String {
        match self {
            &BigNumArg(ref x) => {
                if *x.denom() == num::one() {
                    x.numer().to_str()
                } else {
                    x.to_str()
                }
            }
        }
    }
}

#[deriving(Clone)]
pub struct BoolArg(pub bool);

impl Evaluate for BoolArg {
    fn eval(&self, _: &mut Environment)  -> CalcResult {
        let &BoolArg(x) = self;
        Ok(Boolean(x))
    }

    fn to_symbol(&self) -> String {
        match self {
            &BoolArg(x) => x.to_str()
        }
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

    fn to_symbol(&self) -> String {
        match self {
            &SymbolArg(ref x) => x.to_str()
        }
    }
}

#[deriving(Clone)]
pub struct ProcArg(pub Vec<String>, pub Vec<Token>);

impl Evaluate for ProcArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let &ProcArg(ref x, ref y) = self;
        Ok(Proc(x.clone(), y.clone()))
    }

    fn to_symbol(&self) -> String {
        
        let (param_string, token_string) = match self {
            &ProcArg(ref x, ref y) => {
                (x.iter().fold("".to_str(), |s, p| 
                               s.append(p.to_str().append(" ").as_slice())),
                 y.iter().fold("".to_str(), |s, p| 
                               s.append(p.to_str().append(" ").as_slice())))
            }
        };
        "( ".to_str().append(param_string.append(" , ").as_slice()).append(
            token_string.append(")").as_slice())
    }
}

#[deriving(Clone)]
pub struct VoidArg;

impl Evaluate for VoidArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        Ok(Void)
    }

    fn to_symbol(&self) -> String {
        "".to_str()
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
