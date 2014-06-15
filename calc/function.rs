//! Evaluate functions defined by the user

use super::{CalcResult, Environment, Evaluate};
use super::literal::{Proc, Symbol};
use super::tokenize::{TokenStream, Token, Literal, LParen, RParen, Variable};
//use super::operator;

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<Box<Evaluate>>,
            env: &mut Environment) -> CalcResult {
    
    match env.lookup(fn_name) {
        _ => println!("foo!")
    }

    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, func) = match value {
        Proc(x, y) => (x.clone(), y),
        _ => return Ok(value),
    };

    if args.len() != args_to_fulfill.len() {
        let msg = "fn ".to_str().append(fn_name.as_slice()).append(" takes ")
            .append(args_to_fulfill.len().to_str().as_slice())
            .append(" arguments, but ").append(args.len().to_str().as_slice())
            .append(" arguments were given!");
        return Err(msg);
    }

    let mut child_env = Environment::new_frame(env);

    for (arg_key, arg_val) in args_to_fulfill.iter().zip(args.iter()) {
        child_env.symbols.insert(arg_key.clone(), try!(arg_val.eval(env)));
    }

    func.eval(&mut child_env)
}

fn strip(t: Option<Result<Token, String>>) -> Result<Token, String> {
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

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> Result<(), String> {
    let symbol_vec = try!(get_symbols(tokens));
    assert!(symbol_vec.len() >= 1);
    let symbol = symbol_vec.get(0);

    let nt = try!(strip(tokens.next()));
    let lit = match nt {
        LParen => {
            tokens.index -= 1;
            let expr = try!(super::translate::translate(tokens, env));
            let params: Vec<String> = symbol_vec.tail().iter().map(|x| 
                                                                   x.clone()).collect();
            Proc(params, expr)            
        },
        Literal(lit_ty) => lit_ty,
        Variable(x) => try!(env.lookup(&x)),
        _ => return Err("Invalid parameter value!".to_str())
    };

    match try!(strip(tokens.next())) {
        RParen => {
            env.symbols.insert(symbol.clone(), lit);
            Ok(())
        },            
        _ => Err("Malformed expression!".to_str())
    }
}
