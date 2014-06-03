//!Pretty print just prints the "relevant" information for a result.

use super::literal::*;
use super::{Environment, CalcResult};
use std::num;

pub fn pretty_print(result: &CalcResult, env: &Environment) {
    let success = match result {
        &Ok(ref v)  => v.clone(),
        &Err(ref m) => {
            println!("{}", m);
            return
        }
    };

    match success {
        BigNum(x)  => {
            if *x.denom() == num::one() {
                println!("{}", x.numer())
            } else {
                println!("{}", x)
            }
        },
        Boolean(x) => println!("{}", x),
        Symbol(x)  => {
            let res = match env.vars.find(&x) {
                Some(y) => Ok(y.clone()),
                None    => Err("Not found".to_str())
            };
            print!("{} = ", x);
            pretty_print(&res, env)
        },
        Func(x)    => {
            let (args, fun) = match env.funs.find(&x) {
                Some(&(ref a,ref f))  => (a.clone(), f.clone()),
                None        => {
                    println!("Error: fn {} not found!", x);
                    return
                }
            };
            print!("{} takes ", x);
            for arg in args.iter() {
                match arg {
                    &Symbol(ref a)  => print!("{} ", a),
                    _   => {
                        println!("Error: strange behavior detected!");
                        return
                    }
                }
            }
            match args.len() {
                0 => println!("nothing as arguments and returns the function {}", fun),
                1 => println!("as an argument and applies it to {}", fun),
                _ => println!("as arguments and applies them to {}", fun)
            }
        }

        Void    => { }
    }
}
