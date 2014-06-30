#![crate_id = "rcalc"]
#![crate_type = "bin"]
#![feature(default_type_params, globs, macro_rules)]

//! Polish notation programmable calculator.

#[cfg(target_os = "linux")]
use r_readline::*;

#[cfg(not(target_os = "linux"))]
use rust_no_readline::*;

use calc::{eval, Environment};
use calc::pretty::pretty_print;
use calc::HashMap;
use std::io::{File, Open, ReadWrite};

mod calc;

#[cfg(target_os = "linux")]
pub mod r_readline {
    extern crate libc;

    use self::libc::c_char;
    use std::c_str::CString;
    #[link(name = "readline")]

    extern {
        fn readline(p: *const c_char) -> *const c_char;
        fn add_history(l: *const c_char);
    }

    ///Takes a reference to a string for use as a prompt, and returns an option.
    ///On failure it returns None, which may be the case for ^D or ^C.
    pub fn rust_readline(prompt: &str) -> Option<String> {
        if prompt.len() == 0 { 
            return None
        }

        let c_prompt = prompt.to_c_str();

        c_prompt.with_ref(|c_buf| {
            unsafe {
                let ret_str = CString::new(readline(c_buf), true);
                if ret_str.is_not_null() {
                    ret_str.as_str().map(|ret_str| ret_str.to_str())
                } else {
                    None
                }
            }
        })
    }

    ///Adds a string to a history buffer for use by readline. Does not
    ///take zero length strings.
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
}

#[cfg(not(target_os = "linux"))]
pub mod rust_no_readline {
    use std::io;

    pub fn rust_readline(prompt: &str) -> Option<String> {
        let mut reader = io::stdin();
        match reader.read_line() {
            Ok(x) => Some(x),
            Err(m) => None
        }
    }

    pub fn rust_add_history(line: &str) {
        if line.len() == 0 {
            return
        }

        return
    }
}

fn main() {
    //env will hold all user defined variables and functions in hashmaps,
    //to be looked up when called. They're in the main function for
    //persistence.
    let mut env = Environment::new_global();

    loop {
        let expr = match rust_readline(">>> ") {
            Some(val)   => { val.to_str() }
            None        => { continue }
        };
        rust_add_history(expr.as_slice());

        let exit_or_eval: Vec<&str> = expr.as_slice().words().collect();
        if exit_or_eval.len() == 0 {
            continue
        }

        if expr.as_slice().starts_with(",") {
            special(expr.as_slice(), &mut env);
            continue
        }

        let result = match exit_or_eval.as_slice()[0] {
            "exit" | "(exit" | "(exit)" | ",q" => break,
            "(" => {
                if exit_or_eval.len() >= 2 {
                    match exit_or_eval.as_slice()[1] {
                        "exit" | "exit)"    => break,
                        _   => eval(expr.as_slice().trim(), &mut env),
                    }
                } else {
                    eval(expr.as_slice().trim(), &mut env)
                }
            },

            _   => eval(expr.as_slice().trim(), &mut env)
        };

        println!("{}", pretty_print(&result, &env));
    }
}

fn special(msg: &str, env: &mut Environment) {
    match msg.trim_right() {
        ",clear-state" => {
            env.symbols = HashMap::new();
        },
        ",save-state" => save_state(env),
        ",q" => { },
        _ => println!("There are no other defined actions"),
    }
}

fn save_state(env: &mut Environment) {
    let p = Path::new("state.bytes");
    let mut file = match File::open_mode(&p, Open, ReadWrite) {
        Ok(f) => f,
        Err(e) => {
            println!("{}", e);
            return
        }
    };

    for (key, val) in env.symbols.iter() {
        match file.write_line(format!("{} {}", key, val).as_slice()) {
            Ok(_) => { },
            Err(m) => {
                println!("{}", m);
                return
            }
        }
    }

    println!("Saved to default.txt");
}
