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
    List(Vec<LiteralType>),
    Void
}

#[deriving(Clone)]
pub struct BigNumArg (pub BigRational);

impl Evaluate for BigNumArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let BigNumArg(x) = self.clone(); 
        Ok(BigNum(x))
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
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

    fn to_symbol(&self, _: &mut Environment) -> String {
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

    fn to_symbol(&self, _: &mut Environment) -> String {
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

    fn to_symbol(&self, _: &mut Environment) -> String {
        let (param_string, token_string) = match *self {
            ProcArg(ref x, ref y) => {
                (x.iter().fold("".to_str(), |s, p| format!("{}{} ", s, p)),
                 y.iter().fold("".to_str(), |s, p| format!("{}{} ", s, p)))
            }
        };
        format!("( {}, {})", param_string, token_string)
    }
}

#[deriving(Clone)]
pub struct ListArg(pub Vec<LiteralType>);

impl Evaluate for ListArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let &ListArg(ref x) = self;
        Ok(List(x.clone()))
    }

    fn to_symbol(&self, env: &mut Environment) -> String {
        let list = match self {
            &ListArg(ref x) => x
        };
        list.iter().fold("(".to_str(), |s, y| {
            s = format!("{} {}", s, match trans_literal(y.clone(), env) {
                Ok(t) => t.to_symbol(env),
                Err(m) => m
            });
            s
        }).append(")")
    }
}                

#[deriving(Clone)]
pub struct VoidArg;

impl Evaluate for VoidArg {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        Ok(Void)
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
        "".to_str()
    }
}


/// Translates a literal token into its value
pub fn trans_literal(lit: LiteralType, env: &mut Environment) -> 
    CalcResult<Box<Evaluate>>
    {

    match lit {
        BigNum(x)   => Ok(box BigNumArg(x) as Box<Evaluate>),
        Boolean(x)  => Ok(box BoolArg(x) as Box<Evaluate>),
        Proc(x, y)  => Ok(box ProcArg(x, y) as Box<Evaluate>),
        Symbol(x)   => trans_literal(try!(env.lookup(&x)), env),
        List(x)     => Ok(box ListArg(x) as Box<Evaluate>),
        Void        => Ok(box VoidArg as Box<Evaluate>)
    }
}
