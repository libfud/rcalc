//! Evaluate functions defined by the user

use super::{CalcResult, Environment, Evaluate, ArgType, Atom, arg_to_literal};
use super::literal::{Proc, Symbol};

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &mut Environment) -> CalcResult {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, func) = match value {
        Proc(x, y) => (x, y),
        Symbol(x) => match try!(env.lookup(&x)) {
            Proc(args, f) => (args, f),
            Symbol(_) => return Err("Too much recursion!".to_str()),
            any => return Ok(Atom(any))
        },
        _ => return Ok(Atom(value)),
    };

    if args.len() != args_to_fulfill.len() {
        let msg = format!("fn {} takes {} arguments but {} arguments were given!",
                          fn_name, args_to_fulfill.len(), args.len());
        return Err(msg);
    }

    let mut child_env = Environment::new_frame(env);
    for (key, value) in args_to_fulfill.iter().zip(args.iter()) {
        let val = match try!(arg_to_literal(value, env)) {
            Symbol(x) => try!(env.lookup(&x)),
            otherwise => otherwise,
        };
        println!("{}, {}", key, value);
        child_env.symbols.insert(key.clone(), val);
    }

    if args_to_fulfill.len() > 0 {
        println!("{}", child_env.lookup(args_to_fulfill.get(0)))
    }

    func.eval(&mut child_env)
}
