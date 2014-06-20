//! Evaluate functions defined by the user

use super::expression::Expression;
use super::{CalcResult, Environment, Evaluate, ArgType, Atom, SExpr, arg_to_literal};
use super::literal::{Proc, Void};
use super::tokenize::{TokenStream, Token, LParen, RParen, Variable};

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &mut Environment) -> CalcResult {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, func) = match value {
        Proc(x, y) => (x, y),
        _ => return Ok(Atom(value)),
    };

    if args.len() != args_to_fulfill.len() {
        let msg = format!("fn {} takes {} arguments but {} arguments were given!",
                          fn_name, args_to_fulfill.len(), args.len());
        return Err(msg);
    }

    let mut child_env = Environment::new_frame(env);
    for (key, val) in args_to_fulfill.iter().zip(args.iter()) {
        child_env.symbols.insert(key.clone(), try!(arg_to_literal(val, env)));
    }

    func.eval(&mut child_env)
}

pub fn strip(t: Option<Result<Token, String>>) -> Result<Token, String> {
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
              env: &mut Environment) -> CalcResult<(Vec<String>, Expression)> {
    
    let symbols = try!(get_symbols(tokens));
    let body = match try!(super::translate(tokens, env)) {
        SExpr(x) => x,
        _ => return Err("Not an expression!".to_str()),
    };

    Ok((symbols, body))
}

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> CalcResult {
    let (symbol_vec, body) = try!(lambda(tokens, env));
    if symbol_vec.len() == 0 {
        return Err("That should be impossible, but I'm not sure yet.".to_str())
    }
    let symbol = symbol_vec.get(0);
        
    env.symbols.insert(symbol.clone(), Proc(symbol_vec.tail().to_owned(), body));
    
    Ok(Atom(Void))
}

