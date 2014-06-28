//!List operations.

use super::{ArgType, Atom, CalcResult, Environment, BigRational, Ratio};
use super::bigint::*;
use super::super::literal::{LiteralType, BigNum, List, Proc, Symbol, Boolean};
use super::special::range_getter;
use super::super::{BadArgType, BadNumberOfArgs};
use super::super::expression::Expression;

pub fn proc_getter(args: &Vec<ArgType>, 
                   env: &mut Environment) -> CalcResult<(Vec<String>, Expression)> {
        
    match args.get(0).clone() {
        Atom(Proc(x, y)) => Ok((x.clone(), y.clone())),
        Atom(Symbol(x)) => proc_getter(&vec!(Atom(try!(env.lookup(&x)))), env),
        _ =>  Err(BadArgType(format!("Expected function but found {}", args.get(0))))
    }
}

pub fn create_bigrat(x: int) -> BigRational {
    Ratio::from_integer(x.to_bigint().unwrap())
}


/// Map can handle mapping a function to each element of one or more lists.
pub fn map(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("`map' takes at least two arguments".to_str()))
    }

    let (names, func) = try!(proc_getter(args, env));

    if names.len() == 0 || names.len() != args.tail().len() {
        return Err(BadArgType("Wrong number of arguments for lists supplied".to_str()))
    }

    let mut list_vec: Vec<Vec<LiteralType>> = Vec::new();

    for maybe_list in args.tail().iter() {
        let list = try!(maybe_list.arg_to_literal(env));
        match list {
            List(x) => list_vec.push(x),
            _ => return Err(BadArgType(format!("{} is not a list!", list)))
        }
    }

    let mut result: Vec<LiteralType> = Vec::new();
    let len = list_vec.get(0).len();
    
    for x in range(0u, len) {
        let mut temp: Vec<LiteralType> = Vec::new();
        for y in range(0u, names.len()) {
            if list_vec.get(y).len() != len {
                return Err(BadArgType("Mismatched lengths!".to_str()))
            }
            temp.push(list_vec.as_slice()[y].get(x).clone());
        }

        let mut child_env = Environment::new_frame(env);
        for (name_key, list_val) in names.iter().zip(temp.iter()) {
            child_env.symbols.insert(name_key.clone(), list_val.clone());
        }
        result.push(try!(try!(func.eval(&mut child_env)).arg_to_literal(env)));
    }

    Ok(Atom(List(result)))
}

pub fn reduce(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 3 {
        return Err(BadNumberOfArgs("`reduce' takes at least three arguments".to_str()))
    }

    let (names, fun) = try!(proc_getter(args, env));

    let (x, y) = if names.len() != 2 {
        return Err(BadArgType("Expected 2 names".to_str()))
    } else {
        (names.get(0).clone(), names.get(1).clone())
    };

    let initval = try!(args.get(1).desymbolize(env));

    let list = match try!(args.get(2).desymbolize(env)) {
        List(x) => x.clone(),
        _ => return Err(BadArgType("Invalid type for reduce".to_str()))
    };

    Ok(Atom(try!(reduce_helper(x, y, &initval, list.as_slice(), env, &fun))))
}

pub type LitTy<T = LiteralType> = T;
pub type Env<T = Environment> = T;

pub fn reduce_helper(x: String, y: String, initval: &LitTy, list: &[LitTy], 
                     env: &mut Env, fun: &Expression) -> CalcResult<LitTy> {

    if list.len() == 0 {
        return Err(BadArgType("Cannot fold empty lists!".to_str()))
    }
    
    let mut child_env = Environment::new_frame(env);

    child_env.symbols.insert(x.clone(), initval.clone());
    child_env.symbols.insert(y.clone(), list[0].clone());
    let mut result = try!(try!(fun.eval(&mut child_env)).arg_to_literal(env));
    for val in list.tail().iter() {
        child_env.symbols.insert(x.clone(), result.clone());
        child_env.symbols.insert(y.clone(), val.clone());
        result = try!(try!(fun.eval(&mut child_env)).arg_to_literal(env));
    }

    Ok(result)
}


pub fn filter(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("`filter' takes at least three arguments".to_str()))
    }

    let (names, func) = try!(proc_getter(args, env));

    if names.len() != 1 {
        return Err(BadArgType("Expected 1 name for predicate".to_str()))
    }

    let list = match try!(args.get(1).desymbolize(env)) {
        List(x) => x.clone(),
        _ => return Err(BadArgType("Invalid type for filter".to_str()))
    };

    let mut child_env = Environment::new_frame(env);

    let mut new_list: Vec<LiteralType> = Vec::new();

    for item in list.iter() {
        child_env.symbols.insert(names.get(0).clone(), item.clone());

        match try!(func.eval(&mut child_env)) {
            Atom(Boolean(true)) => new_list.push(item.clone()),
            Atom(Boolean(false)) => { },
            _ => return Err(BadArgType("Invalid predicate type!".to_str()))
        }
    }

    Ok(Atom(List(new_list)))
}

pub fn rangelist(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("`rangelist' requires a beginning and end.".to_str()))
    }

    let (a, b) = (try!(range_getter(try!(args.get(0).desymbolize(env)))),
                  try!(range_getter(try!(args.get(1).desymbolize(env)))));

    if a > b {
        return Err(BadArgType("Bad range: a > b".to_str()))
    }

    Ok(Atom(List(range(a, b).map(|x| BigNum(create_bigrat(x))).collect())))
}

pub fn listlen(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("`list-len' requires a list as an argument".to_str()))
    }

    match try!(args.get(0).desymbolize(env)) {
        List(x) => Ok(Atom(BigNum(create_bigrat(x.len() as int)))),
        x => return Err(BadArgType(format!("`list-len expects a list, {} is not a list!", x)))
    }
}
