//! Special functions like table and plot points.

extern crate num;

use self::num::bigint::*;
use self::num::rational::{Ratio, BigRational};
use std::iter::range_step;
use super::super::literal::{BigNum, List, Void, trans_literal};
use super::listops::proc_getter;
use super::{Evaluate, Environment, CalcResult};
use super::super::tokenize::TokenStream;
use super::super::translate;

pub fn table(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 4 {
        return Err("`table' takes 4 arguments, for function, range and step."
                   .to_str())
    }
    let (name, fun) = try!(proc_getter(args, env));

    if name.len() != 1 {
        return Err("Only single variables are supported currently".to_str())
    }

    let (from, to) = match (try!(args.get(1).eval(env)), 
                            try!(args.get(2).eval(env))) {
        (BigNum(a), BigNum(b)) => (a.to_integer().to_int().unwrap(), 
                                   b.to_integer().to_int().unwrap()),
        _ => return Err("Invalid range!".to_str())
    };

    if from > to {
        return Err("Invalid range: from > to".to_str())
    }

    let step = match try!(args.get(3).eval(env)) {
        BigNum(s) => s.to_integer().to_int().unwrap(),
        _ => return Err("Invalid range!".to_str())
    };

    if step == 0 {
        return Err("Invalid step! Don't use zero!".to_str())
    }

    let mut expr = try!(TokenStream::new_from_tokens(fun));
    let fun_str = expr.expr.clone();

    let mut child_env = Environment::new_frame(env);
    let func = try!(translate::translate(&mut expr, &mut child_env));
                           
    println!("__________");
    println!("|{} | {}|", name.get(0), fun_str);
    println!("¯¯¯¯¯¯¯¯¯¯");
    for x in range_step(from, to + 1, step) {
        let bigr: BigRational = Ratio::from_integer(x.to_bigint().unwrap());
        let temp = BigNum(bigr);
        child_env.symbols.insert(name.get(0).clone(), temp);
        let result = try!(func.eval(&mut child_env));
        let res_str = try!(trans_literal(result, &mut child_env));
        println!("_________");
        println!("|{} | {}", x, res_str.to_symbol(env));
        println!("¯¯¯¯¯¯¯¯¯");
    }

    Ok(Void)
}

pub fn insertion_sort<T: PartialOrd + Clone>(array_orig: &Vec<T>) -> Vec<T> {
    if array_orig.len() <= 1 {
        return array_orig.to_owned()
    }

    let mut array = array_orig.clone();
    let mut i = 1u;
    while i < array.len() {
        let val = array.get(i).clone();
        let mut j = i - 1;
        while j + 1 != 0 && *array.get(j) > val {
            array.as_mut_slice()[j + 1] = array.get(j).clone();
            j -= 1;
        }
        array.as_mut_slice()[j + 1] = val;
        i += 1;
    }

    array
}

pub fn merge<T: PartialOrd + Clone>(left: Vec<T>, right: Vec<T>) -> Vec<T> {
    let mut result: Vec<T> = Vec::new();
    let mut l_index = 0;
    let mut r_index = 0;
    
    while left.len() - l_index > 0 || right.len() - r_index > 0 {
        if left.len() - l_index > 0 && right.len() - r_index > 0 {
            if left.get(l_index) < right.get(r_index) {
                result.push(left.get(l_index).clone());
                l_index += 1;
            } else {
                result.push(right.get(r_index).clone());
                r_index += 1;
            }
        } else if left.len() - l_index > 0 {
            result.push(left.get(l_index).clone());
            l_index += 1;
        } else {
            result.push(right.get(r_index).clone());
            r_index += 1;
        }
    }

    result
}

pub fn merge_sort<T: PartialOrd + Clone>(array: &Vec<T>, 
                                         min_size: uint) -> CalcResult<Vec<T>> {
    if min_size < 1 {
        return Err("0 is an invalid minimum size!".to_str())
    }

    let length = array.len();
    if length <= min_size { 
        return Ok(insertion_sort(array))
    }

    let middle = length / 2;
    let mut left = Vec::from_slice(array.slice(0, middle));
    let mut right = Vec::from_slice(array.slice(middle, length));

    left = try!(merge_sort(&left, min_size));
    right = try!(merge_sort(&right, min_size));

    Ok(merge(left, right))
}
        
pub fn sort(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("Sort takes one and only one list".to_str())
    }

    let list = match try!(args.get(0).eval(env)) {
        List(x) => x.clone(),
        _ => return Err("Cannot sort items which aren't in a list!".to_str())
    };

    if list.iter().any(|x| match *x { BigNum(_) => false, _ => true }) {
        return Err("Sort can only sort numbers!".to_str())
    }

    Ok(List(try!(merge_sort(&list, 100))))
}
