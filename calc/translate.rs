//! something

use std::str;
use std::str::{Slice, Owned};
use super::{CalcResult, Evaluate, Number};
use super::constant::{Constant};
use super::tokenize::{Token, Literal, LParen, RParen, Operator, Name};
use super::tokenize::{LBracket, RBracket, Boolean, BigNum, List};
use super::expression;
use super::expression::{Expression, Function};
use super::function;

pub fn translate(tokens: &[Token]) -> CalcResult<Box<Evaluate>> {
    match (tokens.iter().next(), tokens.iter().rev().next()) {
        (Some(&LParen), Some(&RParen))  => {} //it's good
        _   => return Err(Slice("Badly formatted expression"))
    }

    let top_expr = match tokens[1] {
        Operator(op_type)   => expression::Operator(op_type),
        Name(ref func_name) => Function(try!(function::from_str(func_name.as_slice()))),
        _                   => return Err(Slice("Operator not at beginning of expr!"))
    };

    let mut args: Vec<Box<Evaluate>> = Vec::new();

    let top = tokens.len();
    let mut i = 2u;

    while i < top {
        match tokens[i].clone() {
            // Subexpression begins
            LParen  => {
                let limit = try!(find_rparen(tokens, i, top)) + 1;
                
                //recurse to build subexpressions into AST
                let sub_expr = try!(translate(tokens.slice(i, limit)));

                args.push(sub_expr);

                //skip over the sub expression we just added
                i = limit;
            },

            RParen => {
                //make a new expression based on its type and arguments
                return Ok(box Expression{ expr_type: top_expr, args: args } as Box<Evaluate>)
            },

            LBracket    => {
                let limit = try!(find_rbracket(tokens, i, top)) + 1;
                
                let list = try!(translate(tokens.slice(i, limit)));

                args.push(list);

                i = limit;
            },

            RBracket => {
                return Ok(box Expression{ expr_type: top_expr, args: args } as Box<Evaluate>)
            },

            Operator(op) => {
                return Err(Owned(format!("Operator '{}' in wrong position", op)))
            },

            Literal(literaltype)  => {
                match literaltype {
                    BigNum(x)   => {
                        args.push(box Number(x) as Box<Evaluate>);
                        i += 1;
                    },
                    Boolean(x) => {
                        println!("I dunno yet lol");
                        i += 1;
                    },
                    List(x) => {
                        println!("I dunno yet lol");
                        i += 1;
                    }
                }
            }

            Name(ref c_name) => {
                let constant = box try!(Constant::from_str(c_name.as_slice()));
                args.push(constant as Box<Evaluate>);
                i += 1;
            }
        }
    }

    Err(Slice("Unable to find last parentheses of expression"))
}

// Parens should be counted before anything else to check for malformed
// expressions like (+ 2 2) (+ 2 2). This function does not fulfill that
// task.
fn find_rparen(tokens: &[Token], begin: uint, end: uint) -> Result<uint, str::MaybeOwned> {
    let mut i = begin;
    let mut p_count = 0;
    while i <= end {
        match tokens[i] {
            LParen  => p_count += 1,
            RParen  => p_count -= 1,
            _       => { } //do nothing
        }
        if p_count == 0 {
            return Ok(i);
        }
        i += 1;
    }

    Err(Slice("Parentheses not present or wrongly formatted."))
}

fn find_rbracket(tokens: &[Token], begin: uint, end: uint) -> Result<uint, str::MaybeOwned> {

    let mut i = begin;
    let mut delimiter_count = 0;
    while i <= end {
        match tokens[i] {
            LBracket => delimiter_count += 1,
            RBracket => delimiter_count -= 1,
            _        => { } //do nothing
        }
        if delimiter_count == 0 {
            return Ok(i);
        }
        i += 1;
    }

    Err(Slice("Delimiter not present or wrongly formatted."))
}
