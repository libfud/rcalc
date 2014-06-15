//! The parent module to every other module in calc.

extern crate num;

pub use self::literal::{LiteralType, BigNumArg, BoolArg, SymbolArg};
pub use self::tokenize::{TokenStream, Token};
pub use self::translate::translate;
pub use self::common::help;
pub use std::collections::HashMap;
use std::fmt;
use std::fmt::Show;

pub mod literal;
pub mod tokenize;
pub mod translate;
pub mod expression;
pub mod constant;
pub mod operator;
pub mod function;
pub mod common;
pub mod pretty;

/// A shortcut for the result type that is used everywhere
pub type CalcResult<T = LiteralType> = Result<T, String>;
pub trait Evaluate {
    fn eval(&self, mut env: &mut Environment) -> CalcResult;
/*
    fn show_evaluate(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        self.fmt(f)
    }
*/

/*
    fn clone_evaluate(&self) -> Box<Evaluate> {
        box self.clone() as Box<Evaluate>
    }
*/
}

/// A structure to allow persistence of variables and functions
#[deriving(Clone)]
pub struct Environment {
    pub symbols: HashMap<String, LiteralType>,
    pub parent: Option<Box<Environment>>
}

impl Environment {
    pub fn new_global() -> Environment {
        Environment { symbols:  HashMap::new(), parent: None }
    }

    pub fn new_frame(par: &mut Environment) -> Environment {
        Environment { symbols: HashMap::new(), parent: Some(box par.clone()) }
    }

    pub fn lookup(&self, var: &String) -> CalcResult {
        match self.symbols.find(var) {
            Some(val) => Ok(*val),
            None      => {
                if self.parent.is_some() {
                    match self.parent.unwrap().lookup(var) {
                        Ok(v) => Ok(v),
                        Err(m) => Err(m)
                     }
                } else {
                    Err("Unbound variable!".to_str())
                }
            }
        }
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Environment) -> CalcResult {
    let mut tokens = TokenStream::new(s.to_str());

    let expr = try!(translate(&mut tokens, env));
    expr.eval(env)
}
