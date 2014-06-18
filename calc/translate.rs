//! something

extern crate num;

use super::{CalcResult, Evaluate, Environment, SymbolArg};
use super::constant::Constant;
use super::tokenize::{Literal, LParen, RParen, Operator, Name, Variable, 
                      TokenStream};
use super::expression;
use super::expression::{Expression, ExprType};
use super::function::{strip, lambda, define};
use super::literal::{trans_literal, LiteralType, ListArg, VoidArg, ProcArg};
use super::operator;
use super::operator::{Define, Lambda, Quote, Help, OperatorType};

type Env<T = Environment> = T;
type Expr<T = Box<Evaluate>> = CalcResult<T>;

pub fn begin_expr(tokens: &mut TokenStream) -> Result<(), String> {
    match tokens.next() {
        Some(Ok(LParen)) => Ok(()),
        Some(Ok(_)) => {
            return Err("Incorrectly formatted expression!".to_str());
        },
        Some(Err(msg))      => return Err(msg),
        None                => fail!("Empty expression!")
    }
}

pub fn get_top_expr(tokens: &mut TokenStream) -> Result<ExprType, String> {
    try!(begin_expr(tokens));

    match tokens.next() {
        Some(x) => super::expression::token_to_expr(try!(x)),
        None  => fail!("Empty expression!")
    }
}

pub fn handle_operator(tokens: &mut TokenStream, env: &mut Environment,
                       top_expr: &ExprType, op: OperatorType) -> Expr {
    match *top_expr {
        expression::Operator(Help) => {
            Ok(box SymbolArg(operator::to_str((&op))) as Box<Evaluate>)
        },

        _   => match op {
            Quote => {
                let list = try!(list_it(tokens, env));
                Ok(box ListArg(list) as Box<Evaluate>)
            },
            
            _ => return Err(format!("Operator in wrong place: {}", op))
        }
    }
}

pub fn un_special(etype: ExprType, tokens: &mut TokenStream, env: &mut Env) -> Expr {
    let mut args: Vec<Box<Evaluate>> = Vec::new();
    loop {
        let token = match tokens.next() {
            Some(Ok(x)) => x,
            Some(Err(m)) => return Err(m),
            None => break
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

            RParen => return Ok(Expression::new(etype, args).box_it()),
            
            Operator(op) => args.push(try!(handle_operator(tokens, env, &etype, 
                                                           op))),
                        
            Literal(literaltype)  => {
                args.push(try!(trans_literal(literaltype, env)))
            }

            Name(ref c_name) => {
                let constant = box try!(Constant::from_str(c_name.as_slice()));
                args.push(constant as Box<Evaluate>);
            }
        }
    }

    Err("No closing paren found!".to_str())
}

pub fn list_it(tokens: &mut TokenStream, env: &mut Env) -> 
    CalcResult<Vec<LiteralType>>
{
    try!(begin_expr(tokens));

    let mut lit_vec: Vec<LiteralType> = Vec::new();
    loop {
        let token = try!(strip(tokens.next()));
        match token {
            LParen => {
                tokens.index -= 1;
                let val = try!(translate(tokens, env));
                lit_vec.push(try!(val.eval(env)));
            },
            Literal(lit_ty) => lit_vec.push(lit_ty),
            Variable(x) => lit_vec.push(try!(env.lookup(&x))),
            Name(c) => {
                let constant = try!(Constant::from_str(c.as_slice()));
                lit_vec.push(try!(constant.eval(env)));
            },
            RParen => break,
            _ => return Err(format!("Invalid token for list {}", token)),
        }
    }

    Ok(lit_vec)
}
        

pub fn make_expr(etype: ExprType, tokens: &mut TokenStream, env: &mut Env) -> Expr {
    match etype {
        expression::Operator(Define)    => {
            match define(tokens, env) {
                Ok(_) => { },
                Err(m) => return Err(m)
            }
            return Ok(box VoidArg as Box<Evaluate>)
        },
        expression::Operator(Lambda)    => {
            let (symbols, body) = try!(lambda(tokens));
            return Ok(box ProcArg(symbols, body) as Box<Evaluate>)
        },
        expression::Operator(Quote)     => {
            let list = try!(list_it(tokens, env));
            return Ok(box ListArg(list) as Box<Evaluate>)
        },
        _  => return un_special(etype, tokens, env),
    }
}

pub fn translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    let top_expr = try!(get_top_expr(tokens));
    make_expr(top_expr, tokens, env)
}
