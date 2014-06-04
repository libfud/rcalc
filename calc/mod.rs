//! The parent module to every other module in calc.

extern crate num;
extern crate collections;

pub use self::literal::{LiteralType, BigNumArg, BoolArg, SymbolArg, FunArg};
pub use self::tokenize::TokenStream;
pub use self::translate::translate;
pub use self::common::help;
pub use self::collections::HashMap;

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
}

/// A structure to allow persistence of variables and functions
pub struct Environment {
    pub vars: HashMap<String, LiteralType>,
    pub funs: HashMap<String, (Vec<LiteralType>, String)>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {vars:  HashMap::new(), funs:  HashMap::new() }
    }
}

/// Returns a variable's value from a hashmap, or an error if it doesn't exist
pub fn lookup(var: &String, env: &mut Environment) -> CalcResult {
    match env.vars.find(var) {
        Some(val)   => Ok(val.clone()),
        None        => {
            println!("{}", var);
            Err("value not found!".to_str())
        }
    }
}

/// Returns a tuple of a function's strings and the function itself
pub fn funfind(var: &String, env: &mut Environment) -> Result<(Vec<LiteralType>, String), String> {
    match env.funs.find(var) {
        Some(&(ref args, ref fun))   => Ok((args.clone(), fun.clone())),
        None    => Err("fun not found!".to_str())
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Environment) -> CalcResult {
    let mut tokens = TokenStream::new(s.to_str());

    let expr = try!(translate(&mut tokens, env));
    expr.eval(env)
}
