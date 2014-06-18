//! something

extern crate num;

use super::{CalcResult, Evaluate, Environment, SymbolArg};
use super::constant::Constant;
use super::tokenize::{Literal, LParen, RParen, Operator, Name, Variable, 
                      TokenStream};
use super::expression;
use super::expression::{Expression, Function};
use super::function::{lambda, define};
use super::literal::{trans_literal, VoidArg, ProcArg};
use super::operator;
use super::operator::{Define, Lambda, Help};

pub fn translate(tokens: &mut TokenStream, 
                 env: &mut Environment) -> CalcResult<Box<Evaluate>> {

    match tokens.next() {
        Some(Ok(LParen))    => {}, //good to go
        Some(Ok(_))         => {
            return Err("Incorrectly formatnted expression!".to_str());
        },
        Some(Err(msg))      => return Err(msg),
        None                => fail!("Empty expression!")
    }

    let top_expr_maybe = match tokens.next() {
        Some(Ok(x))     => x,
        Some(Err(msg))  => return Err(msg),
        None            => fail!("Empty expression!")
    };

    let top_expr = match top_expr_maybe {
        Operator(Define)    => {
            match define(tokens, env) {
                Ok(_) => { },
                Err(m) => return Err(m)
            }
            return Ok(box VoidArg as Box<Evaluate>)
        },
        Operator(Lambda)    => {
            let (symbols, body) = try!(lambda(tokens));
            return Ok(box ProcArg(symbols, body) as Box<Evaluate>)
        },
        Operator(op_type)   => expression::Operator(op_type),
        Variable(func_name) => expression::Function(func_name),
        _                   => {
            return Err(("Operator not at beginning of expr!").to_str())
        }
    };

    let mut args: Vec<Box<Evaluate>> = Vec::new();

    loop {
        let token = match tokens.next() {
            Some(Ok(x))     => x,
            Some(Err(msg))  => return Err(msg),
            None            => break
        };

        match token {
            Variable(var) => args.push(box SymbolArg(var) as Box<Evaluate>),

            // Subexpression begins
            LParen  => {
                //reset the counter to account for the lparen
                tokens.index -= 1;
                //recurse to build subexpressions into AST
                let sub_expr = try!(translate(tokens, env));
                args.push(sub_expr);
            },

            RParen => {
                //make a new expression based on its type and arguments
                return Ok(Expression::new(top_expr, args).box_it())
            },

            Operator(op) => {
                match top_expr {
                    expression::Operator(x) => {
                        match x {
                            Help    => {
                                args.push(box SymbolArg(operator::to_str((&op))) 
                                          as Box<Evaluate>)
                            },
                            _   => return Err(
                                format!("Operator in wrong place: {}", x))
                        }
                    },
                    _   => {
                        return Err(format!("Operator in wrong place: {}", op))
                    }
                }
            },

            Literal(literaltype)  => args.push(try!(trans_literal(literaltype,
                                                                           env))),

            Name(ref c_name) => {
                let constant = box try!(Constant::from_str(c_name.as_slice()));
                args.push(constant as Box<Evaluate>);
            }
        }
    }

    Err(("Unable to find last parentheses of expression").to_str())
}
