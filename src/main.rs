#![crate_name = "rcalc"]
#![crate_type = "bin"]
#![feature(default_type_params, globs, macro_rules)]

//! Polish notation programmable calculator.
#[cfg(not(test))]
extern crate types;
#[cfg(not(test))]
use types::{Environment, WithEnv, Atom, SExpr};
#[cfg(target_os = "linux" , not(test))]
use r_readline::*;
#[cfg(not(target_os = "linux"), not(test))]
use rust_no_readline::*;
#[cfg(not(test))]
use calc::eval;
#[cfg(test)]
pub use calc::eval;
use std::task::TaskBuilder;

#[cfg(test)]
mod test;
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

        let c_buf = prompt.to_c_str();

        unsafe {
            let ret_str = readline(c_buf.as_ptr());
            if ret_str.is_not_null() {
                CString::new(ret_str, true).as_str().map(|ret_str| ret_str.to_string())
            } else {
                None
            }
        }
    }

    ///Adds a string to a history buffer for use by readline. Does not
    ///take zero length strings.
    pub fn rust_add_history(line: &str) {
        if line.len() == 0 {
            return
        }
        let c_line = line.to_c_str();
        unsafe {
            add_history(c_line.as_ptr());
        }
    }
}

#[cfg(not(target_os = "linux"))]
pub mod rust_no_readline {
    use std::io;

    pub fn rust_readline(prompt: &str) -> Option<String> {
        let mut reader = io::stdin();
        match reader.read_line() {
            Ok(x) => Some(x),
            Err(_) => None
        }
    }

    pub fn rust_add_history(line: &str) {
        if line.len() == 0 {
            return
        }

        return
    }
}

#[cfg(not(test))]
fn main() {
    let mut env = Environment::new_global();

    loop {
        let expr = match rust_readline(">>> ") {
            Some(val)   => val.to_string(),
            None        => continue,
        };
        rust_add_history(expr.as_slice());

        let exit_q: Vec<&str> = expr.as_slice().words().collect();
        if exit_q.len() == 0 {
            continue
        }

        match exit_q[0] {
            "exit" | "(exit" | "(exit)" | ",q" => break,
            "(" => if exit_q.len() >= 2  && exit_q[1].starts_with("exit") {
                    break
                },
            _   => { },
        }

        let ((exp_tx, exp_rx), (env_tx, env_rx)) = (channel(), channel());
        env_tx.send(env.clone());
        exp_tx.send(expr.as_slice().trim().to_string());

        let ok = TaskBuilder::new().stack_size(9_000_000).try(proc() {
            let mut temp_env = env_rx.recv();
            let expr = exp_rx.recv();
            (eval(expr.as_slice(), &mut temp_env), temp_env)
        });

        match ok {
            Ok((res, new_env)) => {
                env = new_env;
                match res {
                    Ok(Atom(x)) => println!("{}", WithEnv { data: &x, env: &env }),
                    Ok(SExpr(x)) => println!("{}", x),
                    Err(f) => println!("{}", f),
                }
            },
            Err(_) => continue,
        }
    }
}
