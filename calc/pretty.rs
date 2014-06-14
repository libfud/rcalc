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
            let res = match env.symbols.find(&x) {
                Some(y) => Ok(y.clone()),
                None    => Err("Not found".to_str())
            };
            print!("{} = ", x);
            pretty_print(&res, env)
        },
        Proc(x, y) => {
            print!("{}, {}", x, y);
        },
        Void    => { }
    }
}
