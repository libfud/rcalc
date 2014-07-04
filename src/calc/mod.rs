//! The parent module to every other module in calc.

extern crate num;
extern crate types;
extern crate matrix;
extern crate parse;

pub use self::types;
pub use self::types::{ErrorKind, BadArgType, BadNumberOfArgs};
pub use self::matrix::Matrice;
pub use self::num::rational::{BigRational, Ratio};
pub use self::num::bigint;
pub use self::expression::{Atom, SExpr, ArgType};
pub use self::literal::LiteralType;
pub use self::tokenize::{TokenStream, Token};
pub use self::common::help;
pub use std::collections::HashMap;

pub mod matrix;
pub mod literal;
pub mod expression;
pub mod operator;
pub mod function;
pub mod common;
pub mod pretty;


/// A structure to allow persistence of variables and functions

pub fn define(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    use self::expression::Expression;
    use self::literal::{List, Proc, Void, Symbol};
    if args.len() < 2 {
        return Err(BadNumberOfArgs(
            format!("Cannot define a variable without having a name and val")))
    }
    
    let name_and_vars = match try!(args.get(0).desymbolize(env)) {
        List(ref x) => if x.len() == 0 {
            return Err(BadArgType("Name required for definitions".to_str()))
        } else {
            x.clone()
        },
        x => return Err(BadArgType(format!("{} is not a symbol", x)))
    };

    let name = match name_and_vars.get(0) {
        &Symbol(ref x) => x.clone(),
        _ => return Err(BadArgType("Names can only be symbols!".to_str()))
    };

    let vars = if name_and_vars.len() == 1 {
        vec![]
    } else {
        let mut string_vec = Vec::new();
        for arg in name_and_vars.tail().iter() {
            match arg {
                &Symbol(ref x) => string_vec.push(x.clone()),
                _ => return Err(BadArgType("Variables can only be symbols".to_str()))
            }
        }
        string_vec
    };

    if args.len() == 2 {
        match args.last().unwrap() {
            &Atom(ref x) => {
                env.symbols.insert(name, x.clone());
                return Ok(Atom(Void))
            }
            &SExpr(ref x) => {
                match x.eval(env) {
                    Ok(res) => { 
                        env.symbols.insert(name, match res {
                            Atom(y) => y,
                            _ => fail!("Impossible!")
                        });
                    }
                    Err(_) => { env.symbols.insert(name, Proc(vars, x.clone())); }
                }
                return Ok(Atom(Void))
            }
        }
    }

    //there's multiple expressions involved, so we just pack them all that way
    match args.last().unwrap() {
        &Atom(ref x) => {
            env.symbols.insert(name, x.clone());
            Ok(Atom(Void))
        }
        &SExpr(ref x) => {
            let expr = Expression::new(x.expr_type.clone(), args.tail().to_owned());
            env.symbols.insert(name, Proc(vars, expr));
            Ok(Atom(Void))
        }
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Environment) -> CalcResult {
    use self::translate::top_translate;

    let mut tokens = TokenStream::new(s.to_str());

    let expr = try!(top_translate(&mut tokens, env));
    match expr {
        Atom(_) => Ok(expr),
        SExpr(x) => x.eval(env)
    }
}
