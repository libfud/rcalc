//! something

extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Evaluate, Number, BoolArg, MatrixArg};
use super::constant::{Constant};
use super::tokenize::{Token, Literal, LParen, RParen, Operator, Name, LBracket, RBracket};
use super::expression;
use super::expression::{Expression, Function};
use super::function;
use super::literal::{LiteralType, Boolean, BigNum, Matrix};

pub fn translate(tokens: &[Token]) -> CalcResult<Box<Evaluate>> {
    match (tokens.iter().next(), tokens.iter().rev().next()) {
        (Some(&LParen), Some(&RParen))  => {} //it's good
        _   => return Err(("Badly formatted expression").to_strbuf())
    }

    let top_expr = match tokens[1] {
        Operator(op_type)   => expression::Operator(op_type),
        Name(ref func_name) => Function(try!(function::from_str(func_name.as_slice()))),
        _                   => return Err(("Operator not at beginning of expr!").to_strbuf())
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
                
                let matrix = match make_matrix(tokens.slice(i, limit)) {
                    Ok(matrix_vec)  => matrix_vec,
                    Err(msg)        => return Err(msg)
                };

                args.push(box MatrixArg(matrix) as Box<Evaluate>);

                i = limit;
            },

            RBracket => {
                return Err("Mismatching RBrackets!".to_strbuf())
            },

            Operator(op) => {
                return Err(("Operator '"+ op.to_str() + "' in wrong position").to_strbuf())
            },

            Literal(literaltype)  => { 
                match literaltype {
                    BigNum(x)   => {
                        args.push(box Number(x) as Box<Evaluate>);
                        i += 1;
                    },
                    Boolean(x) => {
                        args.push(box BoolArg(x) as Box<Evaluate>);
                        i += 1;
                    },
                    Matrix(x) => {
                        args.push(box MatrixArg(x) as Box<Evaluate>);
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

    Err(("Unable to find last parentheses of expression").to_strbuf())
}

// Parens should be counted before anything else to check for malformed
// expressions like (+ 2 2) (+ 2 2). This function does not fulfill that
// task.
fn find_rparen(tokens: &[Token], begin: uint, end: uint) -> Result<uint, StrBuf> {

    let mut i = begin;
    let mut p_count = 0;

    while i <= end {
        match tokens[i] {
            LParen  => p_count += 1,
            RParen  => p_count -= 1,
            _       => { } //do nothing
        }
        if p_count == 0 { return Ok(i) }

        i += 1;
    }

    Err(("Parentheses not present or wrongly formatted.").to_strbuf())
}

// Needs to be separate because of limitations of match statements
fn find_rbracket(tokens: &[Token], begin: uint, end: uint) -> Result<uint, StrBuf> {

    let mut i = begin;
    let mut delimiter_count = 0;

    while i <= end {
        match tokens[i] {
            LBracket => delimiter_count += 1,
            RBracket => delimiter_count -= 1,
            _        => { } //do nothing
        }
        if delimiter_count == 0 { return Ok(i) }
        i += 1;
    }

    Err(("Delimiter not present or wrongly formatted.").to_strbuf())
}

pub fn make_matrix(tokens: &[Token]) -> Result<Vec<BigRational>, StrBuf> {

    match (tokens.iter().next(), tokens.iter().rev().next()) {
        (Some(&LBracket), Some(&RBracket))  => {} //it's good
        _   => return Err(("Badly formatted expression").to_strbuf())
    }

    let mut matrix: Vec<BigRational> = Vec::new();
    let top = tokens.len();
    let mut i = 1u;

    while i < tokens.len() {
        match tokens[i].clone() {
            // Subexpression begins, potential value for list
            LParen  => {
                let limit = try!(find_rparen(tokens, i, top)) + 1;
                let sub_expr = try!(translate(tokens.slice(i, limit)));
                let evaluated = try!(sub_expr.eval());

                match evaluated {
                    BigNum(x)   => matrix.push(x.clone()),
                    Boolean(x)  => {
                        return Err("Attempted to use a boolean value in a list!".to_strbuf())
                    }
                    Matrix(x)   => {
                        return Err("Nested lists are not allowed".to_strbuf())
                    }
                }

                //skip over the sub expression 
                i = limit;
            },

            RParen => {
                return Err("Malformed list: found unexexted RParen token".to_strbuf())
            },

            LBracket    => {
                return Err("Nested lists are not allowed".to_strbuf())
            },

            RBracket => {
                if matrix.len() == 0 { return Err("Empty lists not allowed!".to_strbuf()) }
                return Ok(matrix)
            },

            Operator(op) => {
                return Err("Operators not allowed in lists".to_strbuf())
            },

            Literal(literaltype)  => { 
                match literaltype {
                    BigNum(x)   => {
                        matrix.push(x.clone());
                        i += 1;
                    },
                    Boolean(x) => {
                        return Err("Booleans not allowed in lists".to_strbuf())
                    },
                    Matrix(x) => {
                        return Err("Nested lists are not allowed".to_strbuf())
                    }
                }
            }

            Name(ref c_name) => {
                return Err("I need to fix constants".to_strbuf())
            }
        }
    }

    Err(("Unable to find last parentheses of expression").to_strbuf())
}
