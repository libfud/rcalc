//! something

use super::{CalcResult, arg_to_literal, Environment};
use super::tokenize::{Literal, LParen, RParen, Operator, Variable, TokenStream};
use super::expression;
use super::expression::{Expression, ExprType, ArgType, Atom, SExpr};
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
    try!(begin_expr(tokens)); //This screws up a lot of things apparently 

    match tokens.next() {
        Some(x) => super::expression::token_to_expr(try!(x)),
        None  => fail!("Empty expression!")
    }
}

pub fn strip<T>(t: Option<Result<T, String>>) -> Result<T, String> {
    match t {
        Some(x) => x,
	None => Err("No names found!".to_str())
    }
}

pub fn get_symbols(tokens: &mut TokenStream) -> Result<Vec<String>, String> {
    fn multi(tokens: &mut TokenStream) -> Result<Vec<String>, String> {
        let mut symbols: Vec<String> = Vec::new();
        loop {
            let nt = try!(strip(tokens.next()));
            match nt {
                Variable(x) => symbols.push(x),
                RParen => break,
                _ => return Err("Unexpected token found in parameters!".to_str()),
            }
        }
        Ok(symbols)
    }
    
    match try!(strip(tokens.next())) {
        LParen => multi(tokens),
        Variable(x) => Ok(vec!(x)),
        _ => return Err("Malformed expression!".to_str()),
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
        Operator(_) => return Err("Invalid body for lambda!".to_str()),
        RParen => return Err("unexpected rparen!".to_str())
    };

    match try!(strip(tokens.next())) {
        RParen => { },
        _ => return Err("No closing paren found!".to_str())
    }

    Ok((symbols, body))
}

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> CalcResult {
    let (symbol_vec, body) = try!(lambda(tokens, env));
    if symbol_vec.len() == 0 {
        return Err("That should be impossible, but I'm not sure yet.".to_str())
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
                    env.symbols.insert(symbol.clone(), try!(arg_to_literal(&result, &mut t_env)));
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
                lit_vec.push(try!(arg_to_literal(&val, env)));
            },
            Literal(lit_ty) => lit_vec.push(lit_ty),
            Variable(x) => lit_vec.push(try!(env.lookup(&x))),
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
