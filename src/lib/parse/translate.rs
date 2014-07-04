//! Translate tokens into expressions and atoms.

use super::{CalcResult, Environment, BadExpr, BadToken, BadArgType,
            Expression, ArgType, Atom, SExpr, LiteralType};
use super::tokenize::{Literal, LParen, RParen, Operator,
                      Variable, Token, TokenStream};
use super::sexpr;
use super::literal::{List, Symbol, Proc};
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

pub fn begin_expr(tokens: &mut TokenStream) -> CalcResult<()> {
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
    use sexpr::Function;

    let dummy_expr_type = Function("dummy".to_str());
    let dumm_expr = try!(make_expr(dummy_expr_type, tokens, env));
    tokens.index -= 1;
    match dumm_expr {
        SExpr(x) => Ok(x.args),
        Atom(_) => fail!("Impossibru")
    }
}

pub fn define(tokens: &mut TokenStream, env: &mut Env) -> CalcResult {
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
        Operator(x) => match x {
            Quote => {
                let list = try!(list_it(tokens, env));
                vec!(Atom(List(list)))
            },    
            _ => return Err(BadToken("Invalid body for define!".to_str())),
        },
        RParen => return Err(BadToken("unexpected rparen!".to_str()))
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

pub fn handle_operator(tokens: &mut TokenStream, env: &mut Env,
                       top_expr: &ExprType, op: OperatorType) -> Expr {
    match *top_expr {
        sexpr::BuiltIn(Help) => {
            Ok(Atom(Symbol(op.op_to_str())))
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
        let token = try!(strip(tokens.next()));

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
                return Err(BadToken("Sorry, gotta pull this feature for now".to_str()))
            }
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
        sexpr::BuiltIn(Define)    => define(tokens, env),
        sexpr::BuiltIn(Lambda)    => {
            let (symbols, body) = try!(lambda(tokens, env));
            match body {
                Atom(_) => Ok(body),
                SExpr(x) => Ok(Atom(Proc(symbols, x))),
            }
        }, 
        sexpr::BuiltIn(Quote)     => {
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
        Err(BadToken("Error: found tokens after end of sexpr".to_str()))
    } else {
        Ok(expr)
    }
}
 
pub fn translate(tokens: &mut TokenStream, env: &mut Env) -> Expr {
    try!(begin_expr(tokens));
    let top_expr = try!(token_to_expr(try!(strip(tokens.next()))));
    make_expr(top_expr, tokens, env)
}
