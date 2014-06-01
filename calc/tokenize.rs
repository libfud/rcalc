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
#[deriving(Show)]
#[deriving(Clone)]
pub enum Token {
    Literal(LiteralType),
    LParen,
    RParen,
    Fun(String),
    Operator(OperatorType),
    Name(String),
    Variable(String),
}

pub struct TokenStream {
    pub expr: String,
    pub index: uint
}

impl Iterator<CalcResult<Token>> for TokenStream {
    fn next(&mut self) -> Option<CalcResult<Token>> {
        loop {
            if self.index == self.expr.len() { 
                return None
            } 

            let temp = self.expr.as_slice().slice_from(self.index);
            if temp.chars().next().unwrap().is_whitespace() {
                self.index += 1;
                continue;
            }

            let token = match temp.chars().next().unwrap() {
                '(' => Some(LParen),
                ')' => Some(RParen),
                _   => None
            };

            if token.is_some() {
                self.index += 1;
                return Some(Ok(token.unwrap()));
            }

            //function . It's pretty blind
            if self.expr.as_slice().slice_from(self.index).starts_with("|") {
                let fn_last = match self.expr.as_slice().slice_from(self.index + 1).find(
                                                                        |c: char| c == '|') {
                    Some(x) => x,
                    None    => {
                        return Some(Err("Function with no limit!".to_str()))
                    }
                };
                let fn_string = self.expr.as_slice().slice(self.index + 1,
                                                    self.index + fn_last + 1).to_str();
                self.index += fn_last + 2;
                return Some(Ok(Fun(fn_string)))
            }

            //Operators
        
            //There is at least one word, so it is safe to unwrap
            let word = self.expr.as_slice().slice_from(self.index).words().next().unwrap();

            //Discard dangling parens
            let word = word.slice(0, word.find(|c: char| c == ')' || c == '(').unwrap_or(word.len()));
            match operator::from_str(word) {
                Some(op_type) => {
                    self.index += word.len();
                    return Some(Ok(Operator(op_type)))
                }
                _       => {}
            }

            //Constants
            match constant::from_const_str(word) {
                Some(c)   => {
                    self.index += word.len();
                    return Some(Ok(Literal(c)))
                },
                _       => {}
            }
        
            //Booleans
       
            let token = match word {
                "true"  => Some(Literal(Boolean(true))),
                "false" => Some(Literal(Boolean(false))),
                _       => None
            };
            if token.is_some() {
                self.index += word.len();
                return Some(Ok(token.unwrap()));
            }

            //Variables
            let c = word.chars().next().unwrap();
            if c.is_alphabetic() {
                self.index += word.len();
                return Some(Ok(Variable(word.to_str())))
            }
 
            //Literals

            //no number should ever start or end with /
            if word.starts_with("/") || word.ends_with("/") {
                return Some(Err("Unrecognized token: ".to_str().append(word.to_str().as_slice())))
            }

            let token = is_number(word);

            if token.is_some() {
                match str_to_rational(token.unwrap().to_str().as_slice()) {
                    Ok(literal_val)    => {
                        self.index += word.len();
                        return Some(Ok(Literal(BigNum(literal_val))))
                    }
                    Err(_)        => {
                        return Some(Err("Unrecognized token!".to_str()))
                    }
                }
            }
       
            //This point is reached if every other kind of token has not been matched
            return Some(Err("Unrecognized token: ".to_str().append(word.to_str().as_slice())))
        }
    }
}

pub fn is_number(word: &str) -> Option<Result<String, String>> {
    let mut negative_sign_counter = 0;
    let mut radix_point_counter = 0;
    let mut fraction_counter = 0;

    for c in word.chars() {
        match c {
            '0'..'9'    => { }, //do nothing here
            '-'         => { negative_sign_counter += 1 },
            '.'         => { radix_point_counter += 1 },
            '/'         => { fraction_counter += 1 },
            _           => {
                return Some(Err("Unrecognized token!".to_str()))
            }
        }
    }

    // Numbers could have a negative sign and, or (exclusively) a divisor or
    // a radix point.
    match (fraction_counter, radix_point_counter, negative_sign_counter) {
        (0, 0, 0) | (1, 0, 0) | (0, 1, 0) => Some(Ok(word.to_str())),

        (0, 0, 1) | (0, 1, 1) | (1, 0, 1) => {
            if word.starts_with("-") == true { 
                Some(Ok(word.to_str()))
            } else {
                Some(Err("Unrecognized token!".to_str()))
            }
        },

        _   => None
    }
}
