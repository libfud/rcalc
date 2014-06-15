//! Evaluate functions defined by the user

use super::{CalcResult, Environment, Evaluate};
use super::literal::{Proc, Symbol};
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
        let msg = "fn ".to_str().append(fn_name.as_slice()).append(" takes ")
            .append(args_to_fulfill.len().to_str().as_slice())
            .append(" arguments, but ").append(args.len().to_str().as_slice())
            .append(" arguments were given!");
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

    let mut lparens = 1;
    let mut rparens = 0;
    let mut token_vec: Vec<Token> = Vec::new();

    loop {
        let nt = try!(strip(tokens.next()));
        match nt {
            LParen => {
                lparens += 1;
                token_vec.push(nt);
            },
            RParen => {
                rparens += 1;
                if rparens == lparens {
                    tokens.index -= 1;
                    break
                }
                token_vec.push(nt);
            }
           
            _ => token_vec.push(nt),
        }
    }

    match try!(strip(tokens.next())) {
        RParen => {
            match token_vec.len() {
                0 => return Err("Malformed Expression!".to_str()),
                1 => {
                    env.symbols.insert(symbol.clone(), match token_vec.get(0) {
                        &Variable(ref x) => Symbol(x.clone()),
                        &Literal(ref lit_ty) => lit_ty.clone(),
                        _ => return Err("Malformed Expression!".to_str()),
                    });
                },
                _ => {
                    env.symbols.insert(symbol.clone(),
                                       Proc(symbol_vec.tail().iter()
                                            .map(|x| x.clone()).collect(), token_vec));
                }                
            }
            Ok(())
        },            
        _ => Err("Malformed expression!".to_str())
    }
}
