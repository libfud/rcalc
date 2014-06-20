//! something

extern crate num;

use super::{CalcResult, Evaluate, Environment};
use super::constant::Constant;
use super::tokenize::{Literal, LParen, RParen, Operator, Name, Variable, 
                      TokenStream};
use super::expression;
use super::expression::{Expression, ExprType, ArgType, Atom, SExpr};
use super::function::{strip, lambda, define};
use super::literal::{LiteralType, Symbol, Proc, List, Void};
use super::operator;
use super::operator::{Define, Lambda, Quote, Help, OperatorType};

type Env<T = Environment> = T;
type Expr<T = ArgType> = CalcResult<T>;

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
            Ok(Atom(Symbol(operator::to_str((&op)))))
        },

        _   => match op {
            Quote => {
                let list = try!(list_it(tokens, env));
                Ok(Atom(List(list)))
            },
            
            _ => return Err(format!("Operator in wrong place: {}", op))
        }
    }
}

pub fn un_special(etype: ExprType, tokens: &mut TokenStream, 
                  env: &mut Env) -> CalcResult<Expression> {
    let mut args: Vec<ArgType> = Vec::new();
    loop {
        let token = match tokens.next() {
            Some(Ok(x)) => x,
            Some(Err(m)) => return Err(m),
            None => break
        };
         
        match token {
            Variable(var) => args.push(Atom(Symbol(var))),

            // Subexpression begins
            LParen  => {
                //reset the counter to account for the lparen
                tokens.index -= 1;
                //recurse to build subexpressions into AST
                let sub_expr = try!(translate(tokens, env));
                args.push(SExpr(sub_expr));
            },

            RParen => return Ok(Expression::new(etype, args)),
            
            Operator(op) => args.push(try!(handle_operator(tokens, env, &etype, 
                                                           op))),
                        
            Literal(literaltype)  => args.push(Atom(literaltype)),

            Name(ref c_name) => {
                let constant = try!(Constant::from_str(c_name.as_slice()));
                args.push(Atom(constant));
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
            Operator(Quote) => {
                let sub_list = try!(list_it(tokens, env));
                lit_vec.push(List(sub_list));
            },
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
            Ok(Atom(Void))
        },
        expression::Operator(Lambda)    => {
            let (symbols, body) = try!(lambda(tokens));
            Ok(Proc(symbols, body))
        },
        expression::Operator(Quote)     => {
            let list = try!(list_it(tokens, env));
            Ok(List(list))
        },
        _  => Ok(SExpr(try!(un_special(etype, tokens, env))))
    }
}

pub fn translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    let top_expr = try!(get_top_expr(tokens));
    make_expr(top_expr, tokens, env)
}
