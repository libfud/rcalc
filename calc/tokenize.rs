//! Tokenizes strings.
/* Largely adapted from Aochagavia's work, with modifications in mind for
 * BigRationals and some other items. Thank you very much, Aochagavia! */

extern crate num;

use super::literal::{LiteralType, Boolean, BigNum, Matrix};
use self::num::rational::BigRational;
use super::common::str_to_rational;
use super::CalcResult;
use super::operator;
use super::operator::{OperatorType};

///Enumeration of valid tokens. Valid tokens are Operators, Literals, LParens,
///RParens, and Names.
#[deriving(Show)]
#[deriving(Clone)]
pub enum Token {
//    Literal(BigRational),
    Literal(LiteralType),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Operator(OperatorType),
    Name(StrBuf)
}

/// Tokenizs a string into 
pub fn tokenize(expr: &str) -> CalcResult<Vec<Token>> {
    let mut tokens = Vec::new();

    let mut i = 0;
    let len = expr.len();

    while i < len {
        let slice = expr.slice_from(i);

        //skip whitespace. unwrap is ok because of while i < len
        if slice.chars().next().unwrap().is_whitespace() {
            i += 1;
            continue;
        }

        //Parentheses and Brackets
        let token = match slice.chars().next().unwrap() {
            '(' => Some(LParen),
            ')' => Some(RParen),
            '[' => Some(LBracket),
            ']' => Some(RBracket),
            _   => None
        };
        if token.is_some() {
            tokens.push(token.unwrap());
            i += 1;
            continue;
        }

        //Operators
        
        //There is at least one word, so it is safe to unwrap
        let word = slice.words().next().unwrap();

        //Discard dangling parens
        let word = word.slice(0, word.find(|c: char| c == ')' || c == '(').unwrap_or(word.len()));

        match operator::from_str(word) {
            Some(op_type) => {
                tokens.push(Operator(op_type));
                i += word.len();
                continue;
            }
            _       => {}
        };
        
        //Booleans
       
        let token = match word {
            "true"  => Some(Literal(Boolean(true))),
            "false" => Some(Literal(Boolean(false))),
            _       => None
        };
        if token.is_some() {
            tokens.push(token.unwrap());
            i += word.len();
            continue;
        }

        //Literals

        //no number should ever start or end with /
        if word.starts_with("/") || word.ends_with("/") {
            return Err(("Unrecognized token '"+ word + "'").to_strbuf())
        }

        let mut negative_sign_counter = 0;
        let mut radix_point_counter = 0;
        let mut fraction_counter = 0;

        for c in word.chars() {
            match c {
                '0'..'9'    => { }, //do nothing here
                '-'         => { negative_sign_counter += 1 },
                '.'         => { radix_point_counter += 1 },
                '/'         => { fraction_counter += 1 },
                _           => { return Err(("Unrecognized token '" + word + "'").to_strbuf()) }
            }
        }

        // Numbers could have a negative sign and, or (exclusively) a divisor or
        // a radix point.
        let token = match (fraction_counter, radix_point_counter, negative_sign_counter) {
            (0, 0, 0) | (1, 0, 0) | (0, 1, 0) => Some(word),

            (0, 0, 1) | (0, 1, 1) | (1, 0, 1) => {
                if word.starts_with("-") == true { Some(word) }
                else { return Err(("Unrecognized token '" + word + "'").to_strbuf()) }
            },

            _   => None
        };
        if token.is_some() {
            match str_to_rational(&[token.unwrap().to_owned()]) {
                Ok(literal_val)    => {
                    tokens.push(Literal(BigNum(literal_val[0])));
                    i += word.len();
                    continue;
                }
                Err(_)        => {
                    return Err(("Unrecognized token '" + word + "'").to_strbuf())
                }
            }
        }

        let c = word.chars().next().unwrap();

        if c.is_alphabetic() {
            tokens.push(Name(word.to_strbuf()));
            i += word.len();
            continue;
        }
        
        //This point is reached if every other kind of token has not been matched
        return Err(("Unrecognized token '"+ word.to_str() + "'").to_strbuf());
    }

    Ok(tokens)
}

