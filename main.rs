#![crate_id = "rcalc"]
#![crate_type = "bin"]
#![feature(default_type_params)]

//! Polish notation calculator.

use std::io;
use calc::eval;
use calc::common::help;

pub mod calc;

fn main() {
    let mut reader = io::stdin();
    let mut expr;

    loop {
        expr = reader.read_line().ok().unwrap_or("exit".to_owned());
        let result;
        let help_exit_or_eval: Vec<&str> = expr.words().collect();
        match help_exit_or_eval.as_slice()[0] {
            "exit" | "(exit" | "(exit)" => { break },

            "help" | "(help" | "(help)" => {
                help(help_exit_or_eval.slice_from(1));
                continue;
            },

            "(" => {
                if help_exit_or_eval.len() >= 2 {
                    match help_exit_or_eval.as_slice()[1] {
                        "exit" | "exit)"    => { break },

                        "help" | "help)"    => {
                            help(help_exit_or_eval.slice_from(2));
                            continue;
                        }

                        _                   => { result = eval(expr.trim()) }
                    }
                }
                else { result = eval(expr.trim()) }
            },

            _   => { result = eval(expr.trim()) }
        }

        match result {
            Err(msg)    => println!("Error: {}", msg),
            Ok(result)  => println!(" {}", result)
        }
    }
}
