//!List operations.

use super::super::{Expression, Evaluate, LiteralType, List, Boolean};
use super::{Atom, ArgType, CalcResult, Environment};
use super::super::{BadArgType, BadNumberOfArgs};

pub type Lit = LiteralType;
pub type LitRes = CalcResult<Lit>;
pub type Env = Environment;
pub type Expr = Expression;

/// Map can handle mapping a function to each element of one or more lists.
pub fn map(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("map".to_string(), "at least".to_string(), 2))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());

    if names.len() == 0 || names.len() != args.tail().len() {
        return Err(BadArgType("Wrong number of arguments for lists supplied".to_string()))
    }

    let mut list_vec: Vec<Vec<Lit>> = Vec::with_capacity(args.tail().len());

    for list in args.tail().iter() {
        list_vec.push(try!(try!(list.desymbolize(env)).to_vec()));
    }

    let mut result: Vec<Lit> = Vec::new();
    let len = list_vec[0].len();
    
    for x in range(0u, len) {
        let mut temp: Vec<Lit> = Vec::new();
        for y in range(0u, names.len()) {
            if list_vec[y].len() != len {
                return Err(BadArgType("Mismatched lengths!".to_string()))
            }
            temp.push(list_vec.as_slice()[y][x].clone());
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
        return Err(BadNumberOfArgs("reduce".to_string(), "at least".to_string(), 3))
    }

    let (names, fun) = try!(try!(args[0].desymbolize(env)).to_proc());

    let (x, y) = if names.len() != 2 {
        return Err(BadArgType("Expected 2 names".to_string()))
    } else {
        (names[0].clone(), names[1].clone())
    };

    let initval = try!(args[1].desymbolize(env));

    let list = try!(try!(args[2].desymbolize(env)).to_vec());

    Ok(Atom(try!(reduce_helper(x, y, &initval, list.as_slice(), env, &fun))))
}

pub fn reduce_helper(x: String, y: String, initval: &Lit, list: &[Lit], 
                     env: &mut Env, fun: &Expr) -> LitRes {

    if list.len() == 0 {
        return Err(BadArgType("Cannot fold empty lists!".to_string()))
    }
    
    let mut child_env = Environment::new_frame(env);

    child_env.symbols.insert(x.clone(), list[0].clone());
    child_env.symbols.insert(y.clone(), initval.clone());
    let mut result = try!(try!(fun.eval(&mut child_env)).arg_to_literal(env));

    for val in list.tail().iter() {
        child_env.symbols.insert(x.clone(), val.clone());
        child_env.symbols.insert(y.clone(), result.clone());

        result = try!(try!(fun.eval(&mut child_env)).arg_to_literal(env));
    }

    Ok(result)
}


pub fn filter(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("filter".to_string(), "at least".to_string(), 3))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());

    if names.len() != 1 {
        return Err(BadArgType("Expected 1 name for predicate".to_string()))
    }

    let list = match try!(args[1].desymbolize(env)) {
        List(x) => x.clone(),
        _ => return Err(BadArgType("Invalid type for filter".to_string()))
    };

    let mut child_env = Environment::new_frame(env);

    let mut new_list: Vec<LiteralType> = Vec::new();

    for item in list.iter() {
        child_env.symbols.insert(names[0].clone(), item.clone());

        match try!(func.eval(&mut child_env)) {
            Atom(Boolean(true)) => new_list.push(item.clone()),
            Atom(Boolean(false)) => { },
            _ => return Err(BadArgType("Invalid predicate type!".to_string()))
        }
    }

    Ok(Atom(List(new_list)))
}

pub fn rangelist(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    use std::iter::range_step;
    use std::num;

    if args.len() < 2 || args.len() > 3 {
        return Err(BadNumberOfArgs("rangelist".to_string(),"at least".to_string(), 2))
    }

//    let (a, b) = (try!(try!(args[0].desymbolize(env)).to_int()),
//                  try!(try!(args[1].desymbolize(env)).to_int()));

    let (a, b) = (try!(args[0].desymbolize(env)), try!(args[1].desymbolize(env)));

    let step = if args.len() == 3 {
//        try!(try!(args[2].desymbolize(env)).to_int())
        try!(args[2].desymbolize(env))
    } else {
        num::one()
    };

    Ok(Atom(List(range_step(a, b, step).collect())))
}
