//! The parent module to every other module in calc.

extern crate num;

pub use self::expression::{arg_to_literal, Atom, SExpr, ArgType};
pub use self::literal::LiteralType;
pub use self::tokenize::{TokenStream, Token};
pub use self::translate::translate;
pub use self::common::help;
pub use std::collections::HashMap;

pub mod literal;
pub mod tokenize;
pub mod translate;
pub mod expression;
pub mod operator;
pub mod function;
pub mod common;
pub mod pretty;

/// A shortcut for the result type that is used everywhere
pub type CalcResult<T = ArgType> = Result<T, String>;

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

    pub fn lookup(&self, var: &String) -> CalcResult<LiteralType> {
        match self.symbols.find(var) {
            Some(val) => Ok(val.clone()),
            None      => {
                if self.parent.is_some() {
                    self.parent.clone().unwrap().lookup(var)
                } else {
                    Err(format!("Unbound variable {}", var))
                }
            }
        }
    }
}

pub fn desymbolize(arg: &ArgType, env: &mut Environment) -> CalcResult<LiteralType> {
    match try!(arg_to_literal(arg, env)) {
        literal::Symbol(x) => env.lookup(&x),
        otherwise => Ok(otherwise)
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Environment) -> CalcResult {
    let mut tokens = TokenStream::new(s.to_str());

    let expr = try!(translate(&mut tokens, env));
    match expr {
        Atom(_) => Ok(expr),
        SExpr(x) => x.eval(env)
    }
}
