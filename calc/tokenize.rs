//! Tokenizes strings.
/* Largely adapted from Aochagavia's work, with modifications in mind for
 * BigRationals and some other items. Thank you very much, Aochagavia! */

extern crate num;

use super::literal::{LiteralType, Boolean, BigNum};
use super::{CalcResult, BadToken, BigRational, BadArgType, Ratio, operator};
use super::operator::{OperatorType};

///Enumeration of valid tokens. Valid tokens are Operators, Literals, LParens,
///RParens, and Names.
#[deriving(Clone, Show, PartialEq)]
pub enum Token {
    Literal(LiteralType),
    LParen,
    RParen,
    Operator(OperatorType),
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

    pub fn peek(&self) -> Option<CalcResult<Token>> {
        self.peek_helper(0)
    }

    fn peek_helper(&self, j: uint) -> Option<CalcResult<Token>> {
        if self.index + j == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
                self.peek_helper(j + 1)
            } else {
                let (token, _) = analyze(self.expr.as_slice().slice_from(self.index + j));
                token
            }
        }
    }
}

pub type MaybeToken<T = Option<CalcResult<Token>>> = (T, uint);

impl Iterator<CalcResult<Token>> for TokenStream {
    fn next(&mut self) -> Option<CalcResult<Token>> {
        if self.index == self.expr.len() {
            return None
        } else {
            if self.expr.as_slice().slice_from(self.index).chars().next().unwrap().is_whitespace() {
                self.index += 1;
                self.next()
            } else {
                let (token, len) = analyze(self.expr.as_slice().slice_from(self.index));
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
            None => Err(BadArgType("Bad numeric encoding".to_str()))
        },

        NonFraction => {
            let floated =  match from_str::<f64>(word) {
                Some(x) => x,
                None => return Err(BadArgType("Bad numeric encoding".to_str()))
            };
            Ok(Ratio::from_float(floated).unwrap())
        },

        Invalid     => Err(BadArgType("Bad numeric encoding".to_str()))
    }
}

/// Determines if a number is represented as a fraction or not.
pub fn get_num_encoding(num_str: &str) -> NumEncoding {
    if num_str.slice_to(1) == "/" || num_str.slice_to(num_str.len() -1) == "/" { 
            return Invalid
    }

    let (divisors, radices) = num_str.chars().fold((0, 0), |(mut x, mut y), c| {
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

pub fn analyze(expr: &str) -> MaybeToken {
    let funs = [is_paren, is_op, is_bool, is_var, is_number];

    for &fun in funs.iter() {
        let (token, len) = fun(expr);
        if token.is_some() {
            return (token, len)
        }
    }

    let word = make_word(expr);
    (Some(Err(BadToken(format!("Unrecognized token: {}", word)))), 0)
}
