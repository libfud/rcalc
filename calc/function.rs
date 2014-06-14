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
        Proc(x, y) => (x.clone(), y.clone()),
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

    let mut tokens = try!(TokenStream::new_from_tokens(func));
    
    let expr = try!(super::translate(&mut tokens, &mut child_env));

    expr.eval(&mut child_env)
}

pub fn define(tokens: &mut TokenStream, env: &mut Environment) -> Result<(), String> {

    let symbol = match tokens.next() {
        Some(Ok(LParen)) => match tokens.next() {
            Some(Ok(RParen)) => return Err("No name for symbol!".to_str()),
            Some(Ok(Variable(x))) => x.clone(),
            Some(Err(m)) => return Err(m),
            None => return Err("Malformed expression!".to_str()),
            _ => return Err("Invalid name!".to_str())
        },
        _ => return Err("For now, everying must be wrapped in parens.".to_str())
    };

    let mut args: Vec<String> = Vec::new();

    loop {
        match tokens.next() {
            None => return Err("Malformed expression!".to_str()),
            Some(Ok(RParen)) => break,
            Some(Ok(Variable(x))) => args.push(x.clone()),
            Some(Err(m)) => return Err(m),
            _ => return Err("Arguments can only be symbols!".to_str())
        }
    }

    let mut body: Vec<Token> = Vec::new();
    loop {
        let val = match tokens.next() {
            None => return Err("Malformed expression!".to_str()),
            Some(Ok(x)) => x.clone(),
            Some(Err(m)) => return Err(m)
        };
        match val {
            RParen => {
                body.push(RParen);
                break
            },
            _ => body.push(val),
        }
    }

    env.symbols.insert(symbol, match body.len() {
        1 => match body.get(1) {
            &Literal(ref lit_ty) => lit_ty.clone(),
            &Variable(ref x) => Symbol(x.clone()),
            _ => return Err("Malformed body!".to_str()),
        },
        _  => Proc(args, body),
    });

            
    Ok(())
}
