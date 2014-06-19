//! Special functions like table and plot points.

extern crate num;

use self::num::bigint::*;
use self::num::rational::{Ratio, BigRational};
use std::iter::range_step;
use super::super::literal::{BigNum, Void, trans_literal};
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
        
        
