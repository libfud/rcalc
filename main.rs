#![crate_id = "rcalc"]
#![crate_type = "bin"]
#![feature(default_type_params)]

//! Polish notation calculator.

use std::io;
use calc::eval;

mod calc;

fn main() {
    let mut reader = io::stdin();
    let mut expr;

    loop {
        expr = reader.read_line().ok().unwrap_or("exit".to_owned());
        if expr.trim() == "exit".to_owned() { break }
        if expr.trim() == "help".to_owned() { println!("Please use (help)"); }
        let output = eval(expr.trim());
        println!("{}", output);
    }
}
