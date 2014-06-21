//! Special functions like table and plot points.

extern crate num;

use self::num::bigint::*;
use self::num::rational::{Ratio, BigRational};
use super::super::literal::{BigNum, List, Void, LiteralType};
use super::listops::proc_getter;
use super::{Environment, CalcResult};
use super::{ArgType, Atom, arg_to_literal};

pub fn range_getter(arg: LiteralType) -> CalcResult<int> {
    match arg {
        BigNum(x) => Ok(x.to_integer().to_int().unwrap()),
        _ => Err("Range and step must be integers!".to_str())
    }
}

pub fn create_bigrat(x: int) -> BigRational {
    Ratio::from_integer(x.to_bigint().unwrap())
}

pub fn table(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 3 || args.len() > 5 {
        return Err("`table' takes at least 3 arguments, with an option for columns"
                   .to_str())
    }

    let (name, func) = try!(proc_getter(args));
    if name.len() != 1 {
        return Err("Only single variables are supported currently".to_str())
    }
    
    let (from, to) = (try!(range_getter(try!(arg_to_literal(args.get(1), env)))),
                      try!(range_getter(try!(arg_to_literal(args.get(2), env)))));
    
    let step = match args.len() {
        3 => 1,
        _ => try!(range_getter(try!(arg_to_literal(args.get(3),env)))),
    };

    let cols = match args.len() {
        3 | 4 => 1,
        _ => try!(range_getter(try!(arg_to_literal(args.get(4), env)))),
    };

    if from >= to || step <= 0 || cols <= 0 {
        return Err("Invalid table command".to_str())
    }

    let fun_str = func.to_str();

    let mut child_env = Environment::new_frame(env);

    let n_len = name.get(0).len();
    let f_len = fun_str.len();
    let top_line = (format!("|{}    |{}{}", name.get(0), fun_str,
                            " ".repeat((60 - (n_len + 4 + f_len)) / cols as uint)))
                    .repeat(cols as uint).append("|");

    println!("{}", "_".repeat(80));
    println!("{}", top_line);
    println!("{}", "¯".repeat(80));

    let mut x = from;
    while x < to + 1 {
        println!("{}", "_".repeat(80));
        for y in range(0, cols) {
            if x + y > to {
                break
            }
            let temp = BigNum(create_bigrat(x + y));
            child_env.symbols.insert(name.get(0).clone(), temp);
            
            let result = try!(func.eval(&mut child_env));
            let res_str = format!("|{}{}|{}", x + y, " ".repeat(5 - (x + y).to_str().len()),
                                  result.to_str());
            print!("{}{}", res_str, " ".repeat((70 - res_str.len()) / cols as uint));
        }
        println!("|");
        println!("{}", "¯".repeat(80));
        x += cols;
    }

    Ok(Atom(Void))
}

pub fn table_list(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() < 2 || args.len() > 3 {
        return Err("`table' takes at least 2 arguments" .to_str())
    }

    let (name, func) = try!(proc_getter(args));
    if name.len() != 1 {
        return Err("Only single variables are supported currently".to_str())
    }

    let list = match try!(arg_to_literal(args.get(1), env)) {
        List(x) => x,
        _ => return Err ("`table-list must take a list as an argument!".to_str())
    };
    
    let cols = match args.len() {
        2 => 1,
        _ => try!(range_getter(try!(arg_to_literal(args.get(2), env))))

    };

    if cols <= 0 {
        return Err("Invalid number of columns".to_str())
    }

    let mut child_env = Environment::new_frame(env);
    let fun_str = func.to_str();

    let n_len = name.get(0).len();
    let f_len = fun_str.len();
    let top_line = (format!("|{}    |{}{}", name.get(0), fun_str,
                            " ".repeat((60 - (n_len + 4 + f_len)) / cols as uint)))
                    .repeat(cols as uint).append("|");

    println!("{}", "_".repeat(80));
    println!("{}", top_line);
    println!("{}", "¯".repeat(80));

    let mut x = 0;
    while x < list.len() {
        println!("{}", "_".repeat(80));
        for y in range(0u, cols as uint) {
            if x + y >= list.len() {
                break
            }
            let temp = list.get(x + y);
            let t_name = temp.to_str();
            child_env.symbols.insert(name.get(0).clone(), temp.clone());
            
            let result = try!(func.eval(&mut child_env));
            let res_str = format!("|{}{}|{}", t_name, " ".repeat(5 - t_name.len()), 
                                  result.to_str());

            print!("{}{}", res_str, " ".repeat((70 - res_str.len()) / cols as uint));
        }
        println!("|");
        println!("{}", "¯".repeat(80));
        x += cols as uint;
    }

    Ok(Atom(Void))
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

    let num_list: Vec<BigRational> = list.move_iter().map(|x| match x {
        BigNum(y) => y,
        _ => fail!("Impossible!".to_str())
    }).collect();

    let answer = try!(merge_sort(&num_list, 100)).move_iter().map(|x| 
                                                                  BigNum(x)).collect();

    Ok(Atom(List(answer)))
}
