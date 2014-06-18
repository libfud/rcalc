//! Evaluate functions defined by the user

use super::{CalcResult, Environment, Evaluate};
use super::literal::{Proc, Symbol, Void};
use super::tokenize::{TokenStream, Token, Literal, LParen, RParen, Variable};
//use super::operator;

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<Box<Evaluate>>,
            env: &mut Environment) -> CalcResult {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, tokens) = match value {
        Proc(x, y) => (x.clone(), y),
        _ => return Ok(value),
    };

    if args.len() != args_to_fulfill.len() {
        let msg = format!("fn {} takes {} arguments but {} arguments were given!",
                          fn_name, args_to_fulfill.len(), args.len());
        return Err(msg);
    }

    let mut child_env = Environment::new_frame(env);
    let mut expr = try!(TokenStream::new_from_tokens(tokens));

    for (arg_key, arg_val) in args_to_fulfill.iter().zip(args.iter()) {
        child_env.symbols.insert(arg_key.clone(), try!(arg_val.eval(env)));
    }

    let func = try!(super::translate(&mut expr, &mut child_env));
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

pub fn get_body(tokens: &mut TokenStream) -> Result<Vec<Token>, String> {
    let mut lparens = 1;
    let mut rparens = 0;
    let mut token_vec: Vec<Token> = Vec::new();

    loop {
        let nt = try!(strip(tokens.next()));
        match nt {
            LParen => lparens += 1,
            RParen => {
                rparens += 1;
                if rparens == lparens {
                    tokens.index -= 1;
                    break
                }
            }
            _ => { },
        }
        token_vec.push(nt);
    }

    match try!(strip(tokens.next())) {
        RParen => Ok(token_vec),
        /*
    */
        _ => Err("Malformed expression!".to_str())
    }
}

pub fn lambda(tokens: &mut TokenStream) -> CalcResult<(Vec<String>, Vec<Token>)> {
    let symbols = try!(get_symbols(tokens));
    let body = try!(get_body(tokens));

    Ok((symbols, body))
}

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> CalcResult {
    let (symbol_vec, body) = try!(lambda(tokens));
    let symbol = symbol_vec.get(0);
        
    match body.len() {
        0 => return Err("Malformed Expression!".to_str()),
        1 => {
            env.symbols.insert(symbol.clone(), match body.get(0) {
                &Variable(ref x) => Symbol(x.clone()),
                &Literal(ref lit_ty) => lit_ty.clone(),
                _ => return Err("Malformed Expression!".to_str()),
            });
        },
        _ => {
            let symbols = symbol_vec.tail().iter().map(|x| x.clone()).collect();
            env.symbols.insert(symbol.clone(), Proc(symbols, body));
        }                
    }

    Ok(Void)
}

