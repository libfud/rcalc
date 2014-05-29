#![crate_id = "rcalc"]
#![crate_type = "bin"]
#![feature(default_type_params)]

//! Polish notation calculator.

extern crate libc;
extern crate collections;

use libc::c_char;
use std::c_str::CString;
use calc::{eval, Environment};
use calc::common::help;
use calc::literal::{BigNum, Boolean, Matrix, Symbol, Func};
use collections::HashMap;
use std::num;

pub mod calc;

#[link(name = "readline")]
extern {
    fn readline(p: *c_char) -> *c_char;
    fn add_history(l: *c_char);
}

pub fn rust_readline(prompt: &str) -> Option<String> {
    if prompt.len() == 0 { return None }
    let c_prompt = prompt.to_c_str();

    c_prompt.with_ref(|c_buf| {
        unsafe {
            let ret_str = CString::new(readline(c_buf), true);
            if ret_str.is_not_null() {
                ret_str.as_str().map(|ret_str| ret_str.to_strbuf())
            } else {
                None
            }
        }
    })
}

pub fn rust_add_history(line: &str) {
    if line.len() == 0 {
        return
    }

    let c_line = line.to_c_str();
    c_line.with_ref(|c_line| {
        unsafe {
            add_history(c_line);
        }
    });
}

fn main() {

    let mut env = Environment {
        vars: HashMap::new(),
        funs: HashMap::new()
    };

    loop {
        let expr = match rust_readline(">>> ") {
            Some(val)   => { val.to_str() }
            None        => { continue }
        };
        rust_add_history(expr.as_slice());

        let help_exit_or_eval: Vec<&str> = expr.as_slice().words().collect();
        if help_exit_or_eval.len() == 0 {
            continue
        }
        let result;

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

                        _   => {
                            result = eval(expr.as_slice().trim(), &mut env);
                        }
                    }
                }
                else {
                    result = eval(expr.as_slice().trim(), &mut env)
                }
            },

            _   => {
                result = eval(expr.as_slice().trim(), &mut env)
            }
        }

        match result {
            Err(msg)    => println!("Error: {}", msg),
            Ok(result)  => match result {
                BigNum(x)  => {
                    if *x.denom() == num::one() {
                        println!("{}",x.numer())
                    } else {
                        println!("{}", x)
                    }
                },
                Boolean(x)  => println!("{}",x),
                Matrix(x)   => println!("{}",x),
                Symbol(x)   => match env.vars.find(&x) {
                    Some(ref y)     => match *y {
                        &BigNum(ref z)  => {
                            if *z.denom() == num::one() {
                                println!("{}", z.numer())
                            } else {
                                println!("{}", z)
                            }
                        },
                        &Matrix(ref z)  => println!("{}", z),
                        &Boolean(ref z) => println!("{}", z),
                        &Symbol(ref z)  => println!("{}", z),
                        &Func(ref s)    => println!("{}", s),
                    },
                    None    => println!("Doesn't exist!")
                },
                Func(x) => println!("{}", x)
            }
        }
    }
}
