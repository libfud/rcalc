//! Evaluate functions defined by the user

use super::{CalcResult, Environment, ArgType, Atom, BadNumberOfArgs};
use super::literal::{Proc, Symbol};

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &mut Environment) -> CalcResult {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, func) = match value {
        Proc(x, y) => (x, y),
        _ => return Ok(Atom(value)),
    };

    if args.len() != args_to_fulfill.len() {
        let msg = format!("fn {} takes {} arguments but {} arguments were given!",
                          fn_name, args_to_fulfill.len(), args.len());
        return Err(BadNumberOfArgs(msg))
    }

    let mut child_env = Environment::new_frame(env);
    for (key, value) in args_to_fulfill.iter().zip(args.iter()) {
        let val = match try!(value.arg_to_literal(env)) {
            Symbol(x) => try!(env.lookup(&x)),
            otherwise => otherwise,
        };
        child_env.symbols.insert(key.clone(), val);
    }

    func.eval(&mut child_env)
}
