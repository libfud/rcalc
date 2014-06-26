//! Special functions like table and plot points.

use super::super::literal::{BigNum, List, Void, LiteralType};
use super::listops::proc_getter;
use super::{Environment, CalcResult};
use super::{ArgType, Atom, arg_to_literal, desymbolize};
use super::super::pretty::{pretty_print, pretty};
use std::{iter, cmp};

pub fn range_getter(arg: LiteralType) -> CalcResult<int> {
    match arg {
        BigNum(x) => Ok(x.to_integer().to_int().unwrap()),
        _ => Err("Range and step must be integers!".to_str())
    }
}

pub fn table(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {    
    if args.len() != 2 {
        return Err("`table' takes a function and a list".to_str())
    }

    let (name, func) = try!(proc_getter(args, env));
    if name.len() != 1 {
        return Err("Only single variables are supported currently".to_str())
    }
    let name = name.get(0);
    
    let list = match try!(desymbolize(args.get(1), env)) {
        List(x) => x,
        _ => return Err("`table' takes a list as its second argument.".to_str())
    };

    let fun_str = func.to_symbol(env);

    let mut child_env = Environment::new_frame(env);

    let (mut table, mut name_len, mut fn_len) = (Vec::new(), name.len(), fun_str.len());
    table.push((name.clone(), fun_str));

    for temp in list.iter() {
        child_env.symbols.insert(name.clone(), temp.clone());
        let t_name = pretty(temp, env);

        if t_name.len() > name_len {
            name_len = t_name.len();
        }
        let result = pretty_print(&func.eval(&mut child_env), env);
        if result.len() > fn_len {
            fn_len = result.len();
        }
        table.push((t_name, result));
    }

    println!("{}", "-".repeat(4 + name_len + fn_len));
    for &(ref x, ref fx) in table.iter() {
        println!("|{a}{b}|{c}{d}|", a = x, b = " ".repeat(name_len - x.len()),
                 d = fx, c = " ".repeat(fn_len - fx.len() + 1));
        println!("{}", "-".repeat(4 + name_len + fn_len));
    }
    
    Ok(Atom(Void))
}

pub fn insertion_sort<T: PartialOrd + Clone>(mut array: Vec<T>) -> Vec<T> {
    if array.len() <= 1 {
        return array.to_owned()
    }

    let mut i = 1u;
    while i < array.len() {
        let val = array.get(i).clone();
        let mut j = i - 1;
        while j + 1 != 0 && array.get(j) > &val {
            array.as_mut_slice()[j + 1] = array.get(j).clone();
            j -= 1;
        }
        array.as_mut_slice()[j + 1] = val;
        i += 1;
    }

    array
}

/*
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
*/

pub fn merge<T: PartialOrd>(left: Vec<T>, right: Vec<T>) -> Vec<T> {
    struct OrderedIterator<T, A, B> {
        a: iter::Peekable<T, A>,
        b: iter::Peekable<T, B>,
    }
    impl<T: PartialOrd, A: Iterator<T>, B: Iterator<T>> Iterator<T> for OrderedIterator<T, A, B> {
        fn next(&mut self) -> Option<T> {
            if self.a.peek().is_none() || 
                self.b.peek().is_none() { self.a.next().or_else(|| self.b.next()) }
            else if self.a.peek() < self.b.peek() { self.a.next() }
            else { self.b.next() }
        }

        fn size_hint(&self) -> (uint, Option<uint>) {
            let (a_min, a_max) = self.a.size_hint();
            let (b_min, b_max) = self.b.size_hint();
            (cmp::max(a_min, b_min), cmp::max(a_max, b_max))
        }
    }

    (OrderedIterator{a:left.move_iter().peekable(), 
                     b: right.move_iter().peekable()}).collect()
}
            

pub fn merge_sort<T: PartialOrd + Clone>(array: Vec<T>, 
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

    left = try!(merge_sort(left, min_size));
    right = try!(merge_sort(right, min_size));

    Ok(merge(left, right))
}
        
pub fn sort(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("Sort takes one and only one list".to_str())
    }

    let list = match try!(arg_to_literal(args.get(0), env)) {
        List(x) => x.clone(),
        _ => return Err("Cannot sort items which aren't in a list!".to_str())
    };

    if list.iter().any(|x| match *x { BigNum(_) => false, _ => true }) {
        return Err("Sort can only sort numbers!".to_str())
    }

    let answer = try!(merge_sort(list, 100));

    Ok(Atom(List(answer)))
}
