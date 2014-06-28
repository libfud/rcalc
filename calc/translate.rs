//! Translate tokens into expressions and atoms.

use super::{CalcResult, Environment, BadExpr, BadToken, BadNumberOfArgs};
use super::tokenize::{Literal, LParen, RParen, Operator, Variable, TokenStream};
use super::expression;
use super::expression::{Expression, ExprType, ArgType, Atom, SExpr};
use super::literal::{LiteralType, Symbol, Proc, List, Void};
use super::operator;
use super::operator::{Define, Lambda, Quote, Help, OperatorType};

pub type Env<T = Environment> = T;
pub type Expr<T = ArgType> = CalcResult<T>;

pub fn begin_expr(tokens: &mut TokenStream) -> CalcResult<()> {
    match tokens.next() {
        Some(Ok(LParen)) => Ok(()),
        Some(Ok(_)) => return Err(BadExpr),
        Some(Err(msg)) => return Err(msg),
        None => fail!("Empty expression!")
    }
}

pub fn get_top_expr(tokens: &mut TokenStream) -> CalcResult<ExprType> {
    try!(begin_expr(tokens)); //This screws up a lot of things apparently 

    match tokens.next() {
        Some(x) => super::expression::token_to_expr(try!(x)),
        None  => Err(BadExpr)
    }
}

pub fn strip<T>(t: Option<CalcResult<T>>) -> CalcResult<T> {
    match t {
        Some(x) => x,
	None => Err(BadExpr)
    }
}

pub fn get_symbols(tokens: &mut TokenStream) -> CalcResult<Vec<String>> {
    fn multi(tokens: &mut TokenStream) -> CalcResult<Vec<String>> {
        let mut symbols: Vec<String> = Vec::new();
        loop {
            let nt = try!(strip(tokens.next()));
            match nt {
                Variable(x) => symbols.push(x),
                RParen => break,
                _ => return Err(BadToken(format!("Unexpected token {}", nt)))
            }
        }
        Ok(symbols)
    }
    
    match try!(strip(tokens.next())) {
        LParen => multi(tokens),
        Variable(x) => Ok(vec!(x)),
        x => Err(BadToken(format!("Unexpected token {}", x)))
    }
}                

pub fn lambda(tokens: &mut TokenStream, 
              env: &mut Environment) -> CalcResult<(Vec<String>, ArgType)> {
    
    let symbols = try!(get_symbols(tokens));
    let body = match try!(strip(tokens.next())) {
        LParen => {
            tokens.index -= 1;
            try!(translate(tokens, env))
        },
        Variable(x) => Atom(Symbol(x)),
        Literal(x) => Atom(x),
        Operator(_) => return Err(BadToken("Invalid body for lambda!".to_str())),
        RParen => return Err(BadToken("unexpected rparen!".to_str()))
    };

    match try!(strip(tokens.next())) {
        RParen => { },
        _ => return Err(BadExpr)
    }

    Ok((symbols, body))
}

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> CalcResult {
    let (symbol_vec, body) = try!(lambda(tokens, env));
    if symbol_vec.len() == 0 {
        return Err(BadNumberOfArgs("That should be impossible, but I'm not sure yet.".to_str()))
    }
    let symbol = symbol_vec.get(0);
        
    match body {
        Atom(x) => {
            env.symbols.insert(symbol.clone(), x);
        },
        SExpr(x) => {
            let mut t_env = env.clone();
            match x.eval(&mut t_env) {
                Ok(result) => {
                    env.symbols.insert(symbol.clone(), try!(result.arg_to_literal(&mut t_env)));
                },
                _ => {
                    env.symbols.insert(symbol.clone(), Proc(symbol_vec.tail().to_owned(), x));
                }
            }
        }
    }
    
    Ok(Atom(Void))
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
            
            _ => return Err(BadToken(format!("Operator in wrong place: {}", op)))
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

            LParen  => {
                tokens.index -= 1;
                let sub_expr = try!(translate(tokens, env));
                args.push(sub_expr);
            },

            RParen => return Ok(Expression::new(etype, args)),

            Operator(op) => args.push(try!(handle_operator(tokens, env, &etype, op))),

            Literal(literaltype)  => args.push(Atom(literaltype)),
        }
    }

    Err(BadExpr)
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
                lit_vec.push(try!(val.arg_to_literal(env)));
            },
            Literal(lit_ty) => lit_vec.push(lit_ty),
            Variable(x) => lit_vec.push(try!(env.lookup(&x))),
            RParen => break,
            Operator(Quote) => {
                let sub_list = try!(list_it(tokens, env));
                lit_vec.push(List(sub_list));
            },
            _ => return Err(BadToken(format!("Invalid token for list {}", token))),
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
            let (symbols, body) = try!(lambda(tokens, env));
            match body {
                Atom(_) => Ok(body),
                SExpr(x) => Ok(Atom(Proc(symbols, x))),
            }
        },
        expression::Operator(Quote)     => {
            let list = try!(list_it(tokens, env));
            Ok(Atom(List(list)))
        },
        _  => Ok(SExpr(try!(un_special(etype, tokens, env))))
    }
}

pub fn translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    let top_expr = try!(get_top_expr(tokens));
    make_expr(top_expr, tokens, env)
}
