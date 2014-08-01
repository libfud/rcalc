//!List operations.

extern crate types;

use self::types::operator::*;
use self::types::{Atom, ArgType, CalcResult, Environment, Expression};
use self::types::literal::{List, Lit, LitRes};
use super::super::{Evaluate, BadArgType, BadNumberOfArgs};

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

pub fn fold(args: &Vec<ArgType>, env: &mut Env, top: XForms) -> CalcResult {
    if args.len() < 3 {
        return Err(BadNumberOfArgs(top.to_string(), "at least".to_string(), 3))
    }

    let (names, fun) = try!(try!(args[0].desymbolize(env)).to_proc());
    let (x, y) = if names.len() != 2 {
        return Err(BadArgType("Expected 2 names".to_string()))
    } else {
        (names[0].clone(), names[1].clone())
    };

    let initval = try!(args[1].desymbolize(env));

    let mut list =  try!(try!(args[2].desymbolize(env)).to_vec());

    if list.len() == 0 {
        return Ok(Atom(initval))
    }

    if top == FoldR { list.reverse(); }

    Ok(Atom(try!(reduce_helper(x, y, &initval, list.as_slice(), env, &fun))))
}

pub fn reduce(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("reduce".to_string(), "at least".to_string(), 2))
    }

    let (names, fun) = try!(try!(args[0].desymbolize(env)).to_proc());
    let (x, y) = if names.len() != 2 {
        return Err(BadArgType("Expected 2 names".to_string()))
    } else {
        (names[0].clone(), names[1].clone())
    };

    let list =  try!(try!(args[1].desymbolize(env)).to_vec());
    if list.len() == 0 {
        Err(BadArgType("Cannot reduce empty lists!".to_string()))
    } else if list.len() == 1 {
        Ok(Atom(list[0].clone()))
    } else {
        Ok(Atom(try!(reduce_helper(x, y, &list[0], list.tail(), env, &fun))))
    }
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

pub fn filter(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("filter".to_string(), "at least".to_string(), 3))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());

    if names.len() != 1 {
        return Err(BadArgType("Expected 1 name for predicate".to_string()))
    }

    let list = try!(try!(args[1].desymbolize(env)).to_vec());

    let mut child_env = Environment::new_frame(env);
    let mut new_list: Vec<Lit> = Vec::new();

    for item in list.move_iter() {
        child_env.symbols.insert(names[0].clone(), item.clone());

        if try!(try!(try!(func.eval(&mut child_env)).desymbolize(env)).to_bool()) {
            new_list.push(item)
        }
    }

    Ok(Atom(List(new_list)))
}

pub fn filter_map(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() < 3 {
        return Err(BadNumberOfArgs("filter-map".to_string(), "only".to_string(), 3))
    }

    let (filter_names, filter) = try!(try!(args[0].desymbolize(env)).to_proc());
    let (map_names, map) = try!(try!(args[1].desymbolize(env)).to_proc());

    if map_names.len() != 1 || filter_names.len() != 1 {
        return Err(BadArgType("Expected 1 variable for predicate and map".to_string()))
    }

    let list = try!(try!(args[2].desymbolize(env)).to_vec());

    let mut child_env = Environment::new_frame(env);
    let mut new_list: Vec<Lit> = Vec::new();

    for item in list.iter() {
        child_env.symbols.insert(filter_names[0].clone(), item.clone());

        if try!(try!(try!(filter.eval(&mut child_env)).desymbolize(env)).to_bool()) {
            child_env.symbols.insert(map_names[0].clone(), item.clone());
            new_list.push(try!(try!(map.eval(&mut child_env)).desymbolize(env)));
        }
    }

    Ok(Atom(List(new_list)))
}

#[inline]
pub fn rangelist(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    use std::iter::range_step;
    use std::num;

    if args.len() < 2 || args.len() > 3 {
        return Err(BadNumberOfArgs("rangelist".to_string(),"at least".to_string(), 2))
    }

    let (a, b) = (try!(args[0].desymbolize(env)), try!(args[1].desymbolize(env)));
    let step = if args.len() == 3 {
        try!(args[2].desymbolize(env))
    } else {
        num::one()
    };

    Ok(Atom(List(range_step(a, b, step).collect())))
}


#[inline]
pub fn sort(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Sort".to_string(), "only".to_string(), 1))
    }
    let mut list = try!(try!(args[0].desymbolize(env)).to_vec());
    list.sort();
    Ok(Atom(List(list)))
}

pub fn sort_by(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    use self::types::operator;

    if args.len() != 2 {
        return Err(BadNumberOfArgs("sort-by".to_string(), "only".to_string(), 2))
    }

    let mut list = try!(try!(args[0].desymbolize(env)).to_vec());

    let order = match try!(try!(args[1].desymbolize(env)).to_proc()) {
        (_, procedure) => match procedure.expr_type { 
            ::types::sexpr::BuiltIn(operator::Ordering(cmp)) => cmp,
            _ => return Err(BadArgType("Use only builtin".to_string()))
        }
    };

    match order {
        operator::Lt => list.sort_by(|a, b| a.cmp(b)),
        operator::Gt => list.sort_by(|a, b| b.cmp(a)),
        _ => return Err(BadArgType("Use only < and >".to_string()))
    }

    Ok(Atom(List(list)))
}

#[inline]
pub fn reverse(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("reverse".to_string(), "only".to_string(), 1))
    }

    let mut list =  try!(try!(args[0].desymbolize(env)).to_vec());
    list.reverse();
    Ok(Atom(List(list)))
}
