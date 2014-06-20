//! Tokenizes strings.
/* Largely adapted from Aochagavia's work, with modifications in mind for
 * BigRationals and some other items. Thank you very much, Aochagavia! */

extern crate num;

use super::literal::{LiteralType, Boolean, BigNum};
use super::common::str_to_rational;
use super::{CalcResult, operator, constant};
use super::operator::{OperatorType};

///Enumeration of valid tokens. Valid tokens are Operators, Literals, LParens,
///RParens, and Names.
#[deriving(Clone, Show, PartialEq)]
pub enum Token {
    Literal(LiteralType),
    LParen,
    RParen,
    Operator(OperatorType),
    Name(String),
    Variable(String),
}

pub struct TokenStream {
    pub expr: String,
    pub index: uint
}

impl TokenStream {
    pub fn new(e: String) -> TokenStream {
        TokenStream { expr: e, index: 0 }
    }
}

pub type MaybeToken<T = Option<CalcResult<Token>>> = (T, uint);

impl Iterator<CalcResult<Token>> for TokenStream {
    fn next(&mut self) -> Option<CalcResult<Token>> {
        if self.index == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().slice_from(self.index).chars()
                .next().unwrap().is_whitespace() {
                self.index += 1;
                self.next()
            } else {
                let (token, len) = analyze(
                    self.expr.as_slice().slice_from(self.index));
                self.index += len;
                token
            }
        }
    }

    //returns the lowest amount of possible remaining tokens,
    //and the most possible remaining tokens
    fn size_hint(&self) -> (uint, Option<uint>) {
        if self.index == self.expr.len() {
            (0, None)
        } else {
            (1, Some(self.expr.len() - self.index))
        }
    }
}

pub fn make_word(expr: &str) -> String {
    let word = expr.words().next().unwrap();
    word.slice(0, word.find(|c: char| c == ')'
               || c == '(').unwrap_or(word.len())).to_str()
}

pub fn is_paren(expr: &str) -> MaybeToken {
    match expr.chars().next().unwrap() {
        '(' => (Some(Ok(LParen)), 1),
        ')' => (Some(Ok(RParen)), 1),
        _   => (None, 0)
    }
}

pub fn is_op(expr: &str) -> MaybeToken {
    let word = make_word(expr);
    match operator::from_str(word.as_slice()) {
        Some(op)    => (Some(Ok(Operator(op))), word.len()),
        _           => (None, 0)
    }
}

pub fn is_const(expr: &str) -> MaybeToken {
    let word = make_word(expr);
    match constant::from_const_str(word.as_slice()) {
        Some(c) => (Some(Ok(Literal(c))), word.len()),
        _       => (None, 0)
    }
}

pub fn is_bool(expr: &str) -> MaybeToken {
    let word = make_word(expr);
    match word.as_slice() {
        "true"  => (Some(Ok(Literal(Boolean(true)))), word.len()),
        "false" => (Some(Ok(Literal(Boolean(false)))), word.len()),
        _       => (None, 0)
    }
}

pub fn is_var(expr: &str) -> MaybeToken {
    let word = make_word(expr);
    let c = word.as_slice().chars().next().unwrap();
    if c.is_alphabetic() {
        (Some(Ok(Variable(word.to_str()))), word.len())
    } else {
        (None, 0)
    }
}

pub fn is_number(expr: &str) -> MaybeToken {
    let word = make_word(expr);

    match str_to_rational(word.as_slice()) {
        Ok(num) => (Some(Ok(Literal(BigNum(num)))), word.len()),
        Err(_)  => (None, 0)
    } 
}

pub fn analyze(expr: &str) -> MaybeToken {
    let funs = [is_paren, is_op, is_const, is_bool, is_var, is_number];

    for &fun in funs.iter() {
        let (token, len) = fun(expr);
        if token.is_some() {
            return (token, len)
        }
    }

    let word = make_word(expr);
    (Some(Err("Unrecognized token: ".to_str().append(word.as_slice()))), 0)
}
