#![crate_id = "rcalc"]
#![crate_type = "bin"]

//! Polish notation calculator.

use std::io;
use eval::eval;

pub mod eval;
pub mod arithmetic;
pub mod trig;
pub mod stats;
pub mod common;
pub mod logic;
pub mod search;

fn main() {
    let mut reader = io::stdin();
    let mut expr;

    loop {
        expr = reader.read_line().ok().unwrap_or("exit".to_owned());
        if expr.trim() == "exit".to_owned() { break }
        let output = eval(expr.trim());
        println!("{}", output);
    }
}
