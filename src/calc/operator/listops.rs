//!List operations.

extern crate types;

use self::types::operator;
use self::types::operator::*;
use self::types::{Atom, ArgType, CalcResult, Environment, Expression};
use self::types::literal::{List, Lit, LitRes};
use super::super::{Evaluate, BadArgType, BadNumberOfArgs};

pub type Env = Environment;
pub type Expr = Expression;

#[inline]
pub fn list_ops(args: &Vec<ArgType>, env: &mut Env, lop: ListOps) -> CalcResult {
    match lop {
        operator::List => list(args, env),
        Cons => cons(args, env), 
        Car => car(args, env),
        Cdr => cdr(args, env),
        Cadr => car(&vec!(try!(cdr(args, env))), env), 
        Cddr => cdr(&vec!(try!(cdr(args, env))), env),
        Caddr => car(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
        Cdddr => cdr(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
    }
}

#[inline]
pub fn transform_ops(args: &Vec<ArgType>, env: &mut Env, top: XForms) -> CalcResult {
    match top {
        Map => map(args, env),
        Reduce |
        Fold | 
        FoldR => fold(args, env, top),
        Filter => filter(args, env),
        FilterMap => filter_map(args, env),
        RangeList => rangelist(args, env), 
        Sort => sort(args, env),
        Reverse => reverse(args, env),
    }
}

pub fn list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut list: Vec<Lit> = Vec::new();
    for arg in args.iter() {
        list.push(try!(arg.arg_to_literal(env)));
    }
    Ok(Atom(List(list)))
}

pub fn cons(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("cons".to_string(), "only".to_string(), 2))
    }

    let car = try!(args[0].arg_to_literal(env));
    let cdr = try!(args[1].arg_to_literal(env));

    match cdr {
        List(x) => Ok(Atom(List(vec!(car).append(x.as_slice())))),
        _ => Ok(Atom(List(vec!(car, cdr))))
    }
}

pub fn car(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("car".to_string(), "only".to_string(), 1))
    }

    match try!(args[0].desymbolize(env)) {
        List(x) => {
            if x.len() < 1 {
                Err(BadArgType("Empty list!".to_string()))
            } else {
                Ok(Atom(x[0].clone()))
            }
        },
        _ => Err(BadArgType("Wrong type for `car'".to_string()))
    }
}

pub fn cdr(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("cdr".to_string(), "only".to_string(), 1))
    }

    match try!(args[0].desymbolize(env)) {
        List(x) => match x.len() {
            0 => Err(BadArgType("List too short!".to_string())),
            _ => Ok(Atom(List(x.tail().to_vec())))
        },
        _ => Err(BadArgType("Wrong type for `cdr'".to_string()))
    }
}

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
    } else if list.len() == 1 {
        return Ok(Atom(list[0].clone()))
    }

    if top == Reduce {
        Ok(Atom(try!(reduce_helper(x, y, &list[0], list.tail(), env, &fun))))
    } else if top == FoldR {
        list.reverse();
        Ok(Atom(try!(reduce_helper(x, y, &initval, list.as_slice(), env, &fun))))
    } else {
        Ok(Atom(try!(reduce_helper(x, y, &initval, list.as_slice(), env, &fun))))
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

#[inline]
pub fn reverse(args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("reverse".to_string(), "only".to_string(), 1))
    }

    let mut list =  try!(try!(args[0].desymbolize(env)).to_vec());
    list.reverse();
    Ok(Atom(List(list)))
}
