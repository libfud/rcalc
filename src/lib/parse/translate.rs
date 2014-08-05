//! Translate tokens into expressions and atoms.

extern crate types;

use self::types::{ErrorKind, BadExpr, BadToken, BadArgType};
use super::{CalcResult, Environment, Expression, ArgType, Atom, SExpr, LiteralType};
use super::{Literal, LParen, RParen, Operator, Variable, Token};
use super::tokenize::TokenStream;
use super::sexpr;
use super::literal::{Lit, List, Symbol, Proc};
use super::sexpr::{BuiltIn, Function, ExprType};
use super::operator::{Define, Lambda, Quote, Help, OperatorType};

pub type Env = Environment;
pub type Expr = CalcResult<ArgType>;

pub fn token_to_expr(token: Token) -> CalcResult<ExprType> {
    match token {
        Variable(x) => Ok(Function(x)),
        Operator(op_ty) => Ok(BuiltIn(op_ty)),
        _ => Err(BadToken(format!(
            "Expected operator or function but found {}", token)))
    }
}

pub fn begin_expr(tokens: &mut TokenStream<Token, ErrorKind>) -> CalcResult<()> {
    match tokens.next() {
        Some(Ok(LParen)) => Ok(()),
        Some(Ok(_)) => return Err(BadExpr),
        Some(Err(msg)) => return Err(msg),
        None => fail!("Empty sexpr!")
    }
}

pub fn strip<T>(t: Option<CalcResult<T>>) -> CalcResult<T> {
    match t {
        Some(x) => x,
	None => Err(BadToken("Expected a token but found nothing!".to_string()))
    }
}

pub fn get_symbols(tokens: &mut TokenStream<Token, ErrorKind>) -> CalcResult<Vec<String>> {    
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

pub fn lambda(tokens: &mut TokenStream<Token, ErrorKind>, 
              env: &Env) -> CalcResult<(Vec<String>, ArgType)> {
    
    let symbols = try!(get_symbols(tokens));
    let body = match try!(strip(tokens.next())) {
        LParen => match tokens.rev(1) {
            Ok(()) => try!(translate(tokens, env)),
            Err(()) => fail!("Unexpected truncation of expression")
        },
        Variable(x) => Atom(Symbol(x)),
        Literal(x) => Atom(x),
        Operator(_) => return Err(BadToken("Invalid body for lambda!".to_string())),
        RParen => return Err(BadToken("unexpected rparen!".to_string()))
    };

    if try!(strip(tokens.peek())) != RParen {
        Err(BadToken(format!("{}", try!(strip(tokens.peek())))))
    } else {
        tokens.next();
        Ok((symbols, body))
    }
}

pub fn expr_accumulator(tokens: &mut TokenStream<Token, ErrorKind>, 
                        env: &Env) -> CalcResult<Vec<ArgType>> {
    let dummy_expr_type = sexpr::Function("dummy".to_string());
    let dumm_expr = try!(make_expr(dummy_expr_type, tokens, env));
    match tokens.rev(1) {
        Ok(()) => { },
        Err(()) => fail!("Unexpected truncation of expression")
    }
    match dumm_expr {
        SExpr(x) => Ok(x.args),
        Atom(_) => fail!("Impossibru")
    }
}

pub fn define(tokens: &mut TokenStream<Token, ErrorKind>, env: &Env) -> CalcResult {
    let symbols: Vec<LiteralType> = try!(
        get_symbols(tokens)).move_iter().map(|x| Symbol(x)).collect();

    if symbols.len() < 1 {
        return Err(BadArgType("Bad number of symbols".to_string()))
    }

    let body = match try!(strip(tokens.next())) {
        LParen => match tokens.rev(1) {
            Ok(()) => try!(expr_accumulator(tokens, env)),
            Err(()) => fail!("Unexpected truncation of expression")
        },
        Variable(x) => vec!(Atom(Symbol(x))),
        Literal(x) => vec!(Atom(x)),
        Operator(x) => match x {
            Quote => vec!(Atom(List(try!(list_it(tokens, env))))),
            _ => return Err(BadToken("Invalid body for define!".to_string())),
        },
        RParen => return Err(BadToken("unexpected rparen!".to_string()))
    };

    if try!(strip(tokens.peek())) == RParen {
        tokens.next();
        let expr = SExpr(Expression::new(sexpr::BuiltIn(Define), 
                                 vec!(Atom(List(symbols))).append(body.as_slice())));
        Ok(expr)
    } else {
        Err(BadToken(format!("{}", try!(strip(tokens.peek())))))
    }
}

pub fn handle_operator(tokens: &mut TokenStream<Token, ErrorKind>, env: &Env,
                       top_expr: &ExprType, op: OperatorType) -> Expr {
    match *top_expr {
        sexpr::BuiltIn(Help) => Ok(Atom(Symbol(op.to_string()))),
        _   => match op {
            Quote => Ok(Atom(List(try!(list_it(tokens, env))))),
            _ => Err(BadToken(format!("Operator in wrong place: {}", op)))
        }
    }
}

pub fn arg_accumulator(etype: &ExprType, tokens: &mut TokenStream<Token, ErrorKind>,
                       env: &Env) -> CalcResult<Vec<ArgType>> {
    let mut args: Vec<ArgType> = Vec::new();

    loop {
        let token = try!(strip(tokens.next()));

        match token {
            Variable(var) => args.push(Atom(Symbol(var))),
            LParen => args.push( match tokens.rev(1) {
                Ok(()) => try!(translate(tokens, env)),
                Err(()) => fail!("Unexpected truncation of expression")
            }),
            RParen => return Ok(args),
            Operator(op) => args.push(try!(handle_operator(tokens, env, etype, op))),
            Literal(lit) => args.push(Atom(lit)),
        }
    }
}

pub fn un_special(etype: ExprType, tokens: &mut TokenStream<Token, ErrorKind>, 
                  env: &Env) -> CalcResult<Expression> {
    let args = try!(arg_accumulator(&etype, tokens, env));
    Ok(Expression::new(etype, args))
}

pub fn list_it(tokens: &mut TokenStream<Token, ErrorKind>, env: &Env) -> CalcResult<Vec<Lit>> {
    try!(begin_expr(tokens));

    let mut lit_vec: Vec<LiteralType> = Vec::new();
    loop {
        let token = try!(strip(tokens.next()));
        match token {
            LParen => return Err(BadToken("Sorry, gotta pull this feature for now".to_string())),
            Literal(lit_ty) => lit_vec.push(lit_ty),
            Variable(x) => lit_vec.push(try!(env.lookup(&x)).clone()),
            RParen => break,
            Operator(Quote) => lit_vec.push(List(try!(list_it(tokens, env)))),
            _ => return Err(BadToken(format!("Invalid token for list {}", token))),
        }
    }

    Ok(lit_vec)
}
        

pub fn make_expr(etype: ExprType, tokens: &mut TokenStream<Token, ErrorKind>, env: &Env) -> Expr {

    match etype {
        sexpr::BuiltIn(Define) => define(tokens, env),
        sexpr::BuiltIn(Lambda) => {
            let (symbols, body) = try!(lambda(tokens, env));
            match body {
                Atom(_) => Ok(body),
                SExpr(x) => Ok(Atom(Proc(symbols, x))),
            }
        }, 
        sexpr::BuiltIn(Quote) => Ok(Atom(List(try!(list_it(tokens, env))))),
        _  => Ok(SExpr(try!(un_special(etype, tokens, env))))
    }
}

pub fn top_translate(tokens: &mut TokenStream<Token, ErrorKind>, env: &Env) -> Expr {
    let expr = try!(translate(tokens, env));
    if tokens.next().is_some() {
        Err(BadToken("Error: found tokens after end of sexpr".to_string()))
    } else {
        Ok(expr)
    }
}
 
pub fn translate(tokens: &mut TokenStream<Token, ErrorKind>, env: &Env) -> Expr {
    try!(begin_expr(tokens));
    let top_expr = try!(token_to_expr(try!(strip(tokens.next()))));
    make_expr(top_expr, tokens, env)
}
