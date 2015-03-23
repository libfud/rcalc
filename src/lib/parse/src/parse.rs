#![crate_name = "parse"]
#![crate_type = "lib"]
#![feature(core,str_words,collections)]

extern crate num;
extern crate types;

pub use num::rational::{BigRational, Ratio};
pub use types::{CalcResult, Environment, Expression, ArgType, LiteralType, ErrorKind};
pub use types::ArgType::{Atom,SExpr};
pub use types::ErrorKind::{BadToken, BadExpr, BadArgType};
pub use types::{sexpr, operator, literal};
use types::literal::LiteralType::{BigNum, Boolean};
use operator::OperatorType;
pub use tokenize::{TokenStream, MaybeToken};
use translate::top_translate;
use std::fmt;

mod tokenize;
mod translate;

#[derive(Clone, PartialEq)]
pub enum Token {
    Literal(LiteralType),
    LParen,
    RParen,
    Operator(OperatorType),
    Variable(String),
}

impl fmt::Display for Token {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            Token::Literal(ref x) => x.to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::Operator(ref x) => x.to_string(),
            Token::Variable(ref x) => x.clone(),
        }));
        Ok(())
    }
} 

pub fn make_word(expr: &str) -> String {
    let word = expr.words().next().unwrap();
    word[0 .. word.find(|c: char| c == ')' || c == '(').unwrap_or(word.len())].to_string()
}

pub fn is_paren(expr: &str) -> MaybeToken<Token, ErrorKind> {
    match expr.chars().next().unwrap() {
        '(' => (Some(Ok(Token::LParen)), 1),
        ')' => (Some(Ok(Token::RParen)), 1),
        _   => (None, 0)
    }
}

pub fn is_op(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    match word.as_slice().parse::<OperatorType>() {
        Ok(op) => (Some(Ok(Token::Operator(op))), word.len()),
        _      => (None, 0)
    }
}

pub fn is_bool(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    match word.as_slice() {
        "true"  => (Some(Ok(Token::Literal(Boolean(true)))), word.len()),
        "false" => (Some(Ok(Token::Literal(Boolean(false)))), word.len()),
        _       => (None, 0)
    }
}

pub fn is_var(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    let c = word.as_slice().chars().next().unwrap();
    if c.is_alphabetic() {
        (Some(Ok(Token::Variable(word.to_string()))), word.len())
    } else {
        (None, 0)
    }
}

pub fn is_number(expr: &str) -> MaybeToken<Token, ErrorKind> {
    let word = make_word(expr);
    match str_to_rational(word.as_slice()) {
        Ok(num) => (Some(Ok(Token::Literal(BigNum(num)))), word.len()),
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
        NumEncoding::Fraction    => match word.parse::<BigRational>() {
            Ok(x) => Ok(x),
            Err(_) => Err(BadArgType("Bad numeric encoding".to_string()))
        },

        NumEncoding::NonFraction => match word.parse::<f64>() {
            Ok(x) => Ok(Ratio::from_float(x).unwrap()),
            Err(_) =>  Err(ErrorKind::BadArgType("Bad numeric encoding".to_string()))
        },

        NumEncoding::Invalid     => Err(BadArgType("Bad numeric encoding".to_string()))
    }
}

/// Determines if a number is represented as a fraction or not.
pub fn get_num_encoding(num_str: &str) -> NumEncoding {
    if &num_str[.. 1] == "/" || &num_str[.. (num_str.len() -1)] == "/" { 
            return NumEncoding::Invalid
    }

    let (divisors, radices) = num_str.chars().fold((0usize, 0usize), |(mut x, mut y), c| {
        if c == '/' {
            x += 1
        } else if c == '.' {
            y += 1
        }
        (x, y)
    });

    match (divisors, radices) {
        (0, 0) | (0, 1) => NumEncoding::NonFraction,
        (1, 0)          => NumEncoding::Fraction,
        _               => NumEncoding::Invalid
    }
}

pub fn parse(s: &str, env: &Environment) -> CalcResult {
    let rules: Vec<fn(&str) -> (Option<Result<Token, ErrorKind>>, usize)> = 
        vec!(is_paren, is_op, is_bool, is_var, is_number);
    let mut tokens = TokenStream::new(s.to_string(), rules, BadToken("Unrecognized token".to_string()));
    top_translate(&mut tokens, env)
}
