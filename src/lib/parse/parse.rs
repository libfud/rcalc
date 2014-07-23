#![crate_name = "parse"]
#![crate_type = "lib"]
#![feature(default_type_params)]

extern crate num;
extern crate types;

use std::rc::Rc;
pub use num::rational::{BigRational, Ratio};
pub use types::{CalcResult, Environment, BadToken, BadExpr, BadArgType,
                Expression, ArgType, Atom, SExpr, LiteralType};
pub use types::{sexpr, operator, literal, ErrorKind};
use types::literal::{BigNum, Boolean};

use operator::OperatorType;
pub use tokenize::{TokenStream, MaybeToken};
use translate::top_translate;

mod tokenize;
mod translate;

#[deriving(Clone, Show, PartialEq)]
pub enum Token {
    Literal(LiteralType),
    LParen,
    RParen,
    Operator(OperatorType),
    Variable(String),
}

pub fn make_word(expr: &str) -> String {
    let word = expr.words().next().unwrap();
    word.slice(0, word.find(|c: char| c == ')'
               || c == '(').unwrap_or(word.len())).to_string()
}

pub fn is_paren(expr: &str) -> MaybeToken<Token, ErrorKind> {
    match expr.chars().next().unwrap() {
        '(' => (Some(Ok(LParen)), 1),
        ')' => (Some(Ok(RParen)), 1),
        _   => (None, 0)
    }
}

pub fn is_op(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    match from_str::<OperatorType>(word.as_slice()) {
        Some(op)    => (Some(Ok(Operator(op))), word.len()),
        _           => (None, 0)
    }
}

pub fn is_bool(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    match word.as_slice() {
        "true"  => (Some(Ok(Literal(Boolean(true)))), word.len()),
        "false" => (Some(Ok(Literal(Boolean(false)))), word.len()),
        _       => (None, 0)
    }
}

pub fn is_var(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    let c = word.as_slice().chars().next().unwrap();
    if c.is_alphabetic() {
        (Some(Ok(Variable(word.to_string()))), word.len())
    } else {
        (None, 0)
    }
}

pub fn is_number(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);

    match str_to_rational(word.as_slice()) {
        Ok(num) => (Some(Ok(Literal(BigNum(num)))), word.len()),
        Err(_)  => (None, 0)
    } 
}

/// Enumeration of ways to write numbers.
pub enum NumEncoding {
    Fraction,
    NonFraction,
    Invalid
}

/// Converts a string into a bigrational.
pub fn str_to_rational(word: &str) -> CalcResult<BigRational> {

    let number_type = get_num_encoding(word);
    match number_type {
        Fraction    => match from_str::<BigRational>(word) {
            Some(x) => Ok(x),
            None => Err(BadArgType("Bad numeric encoding".to_string()))
        },

        NonFraction => {
            let floated =  match from_str::<f64>(word) {
                Some(x) => x,
                None => return Err(BadArgType("Bad numeric encoding".to_string()))
            };
            Ok(Ratio::from_float(floated).unwrap())
        },

        Invalid     => Err(BadArgType("Bad numeric encoding".to_string()))
    }
}

/// Determines if a number is represented as a fraction or not.
pub fn get_num_encoding(num_str: &str) -> NumEncoding {
    if num_str.slice_to(1) == "/" || num_str.slice_to(num_str.len() -1) == "/" { 
            return Invalid
    }

    let (divisors, radices) = num_str.chars().fold((0u, 0u), |(mut x, mut y), c| {
        if c == '/' {
            x += 1
        } else if c == '.' {
            y += 1
        }
        (x, y)
    });

    match (divisors, radices) {
        (0, 0) | (0, 1) => NonFraction,
        (1, 0)          => Fraction,
        _   => Invalid
    }
}

pub fn parse(s: &str, env: &Rc<Environment>) -> CalcResult {
    let rules = vec!(is_paren, is_op, is_bool, is_var, is_number);
    let mut tokens = TokenStream::new(s.to_string(), rules, /*Token, */
                                      BadToken("Unrecognized token".to_string()));
    top_translate(&mut tokens, env)
}
