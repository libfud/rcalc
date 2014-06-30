//! Translate tokens into expressions and atoms.

use super::{CalcResult, Environment, BadExpr, BadToken};
use super::tokenize::{Literal, LParen, RParen, Operator, Variable, TokenStream};
use super::expression;
use super::expression::{Expression, ExprType, ArgType, Atom, SExpr};
use super::literal::{LiteralType, Symbol, Proc, List};
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

pub fn strip<T>(t: Option<CalcResult<T>>) -> CalcResult<T> {
    match t {
        Some(x) => x,
	None => Err(BadToken("Expected a token but found nothing!".to_str()))
    }
}

pub fn get_symbols(tokens: &mut TokenStream) -> CalcResult<Vec<String>> {    
    match try!(strip(tokens.next())) {
        LParen => {
            let mut symbols: Vec<String> = Vec::new();
            loop {
                match try!(strip(tokens.next())) {
                    Variable(x) => symbols.push(x),
                    RParen => break,
                    x => return Err(BadToken(format!("Unexpected token {}", x)))
                }
            }
            Ok(symbols)
        }
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

    if try!(strip(tokens.peek())) != RParen {
        Err(BadToken(format!("{}", try!(strip(tokens.peek())))))
    } else {
        tokens.next();
        Ok((symbols, body))
    }
}

pub fn expr_accumulator(tokens: &mut TokenStream, env: &mut Env) -> CalcResult<Vec<ArgType>> {
    use super::expression::Function;

    let dummy_expr_type = Function("dummy".to_str());
    let dumm_expr = try!(make_expr(dummy_expr_type, tokens, env));
    tokens.index -= 1;
    match dumm_expr {
        SExpr(x) => Ok(x.args),
        Atom(_) => fail!("Impossibru")
    }
}

pub fn define(tokens: &mut TokenStream, env: &mut Env) -> CalcResult {
    use super::BadArgType;

    let symbols: Vec<LiteralType> = try!(
        get_symbols(tokens)).move_iter().map(|x| Symbol(x)).collect();

    if symbols.len() < 1 {
        return Err(BadArgType("Bad number of symbols".to_str()))
    }

    let body = match try!(strip(tokens.next())) {
        LParen => {
            tokens.index -= 1;
            try!(expr_accumulator(tokens, env))
        },
        Variable(x) => vec!(Atom(Symbol(x))),
        Literal(x) => vec!(Atom(x)),
        Operator(_) => return Err(BadToken("Invalid body for lambda!".to_str())),
        RParen => return Err(BadToken("unexpected rparen!".to_str()))
    };

    if try!(strip(tokens.peek())) == RParen {
        tokens.next();
        let expr = SExpr(Expression::new(expression::Operator(Define), 
                                 vec!(Atom(List(symbols))).append(body.as_slice())));
        Ok(expr)
    } else {
        Err(BadToken(format!("{}", try!(strip(tokens.peek())))))
    }
}

pub fn handle_operator(tokens: &mut TokenStream, env: &mut Env,
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

pub fn arg_accumulator(etype: &ExprType, tokens: &mut TokenStream,
                       env: &mut Env) -> CalcResult<Vec<ArgType>> {
    let mut args: Vec<ArgType> = Vec::new();

    loop {
        let token = match tokens.next() {
            Some(Ok(x)) => x,
            Some(Err(m)) => return Err(m),
            None => return Err(BadExpr)
        };

        match token {
            Variable(var) => args.push(Atom(Symbol(var))),
            LParen => {
                tokens.index -= 1;
                let sub_expr = try!(translate(tokens, env));
                args.push(sub_expr);
            },
            RParen => return Ok(args),
            Operator(op) => args.push(try!(handle_operator(tokens, env, etype, op))),
            Literal(lit) => args.push(Atom(lit)),
        }
    }
}

pub fn un_special(etype: ExprType, tokens: &mut TokenStream, 
                  env: &mut Env) -> CalcResult<Expression> {
    let args = try!(arg_accumulator(&etype, tokens, env));
    Ok(Expression::new(etype, args))
}

pub fn list_it(tokens: &mut TokenStream, env: &mut Env) -> CalcResult<Vec<LiteralType>> {
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
        expression::Operator(Define)    => define(tokens, env),
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

pub fn top_translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    let expr = try!(translate(tokens, env));
    if tokens.next().is_some() {
        println!("{}", tokens.expr.as_slice().slice_from(tokens.index - 1));
        Err(BadToken("Error: found tokens after end of expression".to_str()))
    } else {
        Ok(expr)
    }
}
 
pub fn translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    try!(begin_expr(tokens));
    let top_expr = try!(super::expression::token_to_expr(try!(strip(tokens))));
    make_expr(top_expr, tokens, env)
}
