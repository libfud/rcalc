//! Evaluate functions defined by the user

extern crate collections;

use self::collections::HashMap;
use super::{CalcResult, funfind, Environment, Evaluate};
use super::literal::{LiteralType, BigNum, Boolean, Symbol, Func, Void};
use super::tokenize::{TokenStream, Literal, LParen, RParen, Operator, Variable, Token};
use super::operator::unbox_it;
use super::operator;

///Returns a function's name if it's already defined, or an error if it is not found.
pub fn from_str(name: &str, env: &mut Environment) -> CalcResult<String> {
    match funfind(&name.to_str(), env) {
        Ok(_)   => Ok(name.to_str()),
        Err(_)  => Err("Unknown function: ".to_str().append(name.to_str().as_slice()))
    }
}

pub fn token_to_str(token: Token, sub_map: &HashMap<String, String>) -> Result<String, String> {
    match token {
        LParen      => Ok("( ".to_str()),
        RParen      => Ok(") ".to_str()),
        Literal(x)  => match x {
            BigNum(y)   => Ok(y.to_str().append(" ")),
            Boolean(y)  => Ok(y.to_str().append(" ")),
            _   => fail!("Unexpected type!")
        },
        Operator(x) => Ok(operator::to_str(&x).append(" ")),
        Variable(x) => { 
            let x_to_str = match sub_map.find(&x) {
                Some(val)   => val.clone(),
                None        => {
                    return Err("Variable used in expression not dislcosed in args!".to_str())
                }
            };
            Ok(x_to_str.append(" "))
        },
        _   => return Err("Unexpected type for function!".to_str())
    }
}

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    let (args_to_fulfill, funcstr) = try!(funfind(fn_name, env));

    if args.len() != args_to_fulfill.len() {
        return Err("Improper list of arguments!".to_str())
    }

    //Reconvert Symbol(String) to String, append to vector for use in hashmap
    let args_strs: Vec<String> = args_to_fulfill.move_iter().map(|x| match x {
        Symbol(x)   => x,
        _   => fail!("Unexpected argument type!")
    }).collect();

    //Unbox the substitutions.
    let subs_literals = try!(unbox_it(args, env));

    let subs_strings: Vec<String> = subs_literals.iter().map(|x| match *x {
        BigNum(ref y)   => y.to_str(),
        Boolean(ref y)  => y.to_str(),
        _   => fail!("unbox it returned something it shouldn't have!")
    }).collect();

    //Populate a hashmap with the arguments as keys as the substition strings as values,
    //and use it to rewrite the expression
    let sub_map = args_strs.move_iter().zip(subs_strings.move_iter()).collect();

    let mut tokens = TokenStream {
        expr: funcstr,
        index: 0u
    };

    let mut evaluable_string: String = String::new();

    for token in tokens {
        let sub_str = try!(token_to_str(try!(token), &sub_map));
        
        evaluable_string = evaluable_string.append(sub_str.as_slice());
    }
    
    super::eval(evaluable_string.as_slice(), env)
}

pub fn get_name(expr: &str) -> Result<String, String> {
    let name = expr.words().next().unwrap();
    if !name.chars().next().unwrap().is_alphabetic() {
        return Err("illegal name!".to_str())
    }

    if name.len() == expr.len() {
        return Err("Empty function!".to_str())
    }

    Ok(name.to_str())
}

pub fn get_args(expr: &str) -> Result<(Vec<LiteralType>, uint), String> {
    if !expr.starts_with("(") {
        return Err("Improperly formatted arguments!".to_str())
    }

    let args_len = match expr.find(|c: char| c == ')') {
        Some(x) => x,
        None    => return Err("No body found after arguments!".to_str())
    };

    if args_len == expr.len() {
        return Err("No body found after arguments!".to_str())
    }

    let args = expr.slice(1, args_len + 1).words().map(|arg|
        if arg.ends_with(")") {
            Symbol(arg.slice_to(arg.len() -1).to_str())
        } else {
            Symbol(arg.to_str())
        }
    ).collect();

    Ok((args, args_len))
}

pub fn defun(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("bad use of defun!".to_str())
    }

    let fn_string = match try!(args.get(0).eval(env)) {
        Func(x) => x.as_slice().trim().to_str(),
        _       => return Err("attempted illegal defunition!".to_str())
    };

    if fn_string.len() == 0 {
        return Err("That's not a function! That's not anything!".to_str())
    }

    let name = try!(get_name(fn_string.as_slice()));

    let expr = fn_string.as_slice().slice_from(name.len()).trim();
    if expr.len() == 0 {
        return Err("No arguments or body found!".to_str())
    }

    let (args, args_len) = try!(get_args(expr));

    let body = expr.slice_from(args_len + 1).trim();
    if body.len() == 0 || body.starts_with("(") == false || body.ends_with(")") == false {
        println!("{}", body);
        return Err("No proper procedure found!".to_str())
    }

    env.funs.insert(name, (args, body.to_str()));

    Ok(Void)
}
