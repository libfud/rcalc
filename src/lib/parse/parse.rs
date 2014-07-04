#![crate_id = "sexpr-parse"]
#![crate_type = "lib"]
#![feature(default_type_params)]

extern crate types;

pub use types::{CalcResult, Environment, BadToken, BadExpr, BadArgType,
                Ratio, BigRational, Expression, ArgType, Atom, SExpr,
                LiteralType};
pub use types::{sexpr, operator, literal};

use tokenize::TokenStream;
use translate::top_translate;

mod tokenize;
mod translate;

pub fn parse(s: &str, env: &mut Environment) -> CalcResult {
    let mut tokens = TokenStream::new(s.to_str());
    top_translate(&mut tokens, env)
}
