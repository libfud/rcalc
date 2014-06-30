//! Evaluate functions defined by the user

use super::{CalcResult, Environment, ArgType, Atom, BadNumberOfArgs};
use super::literal::{Proc};

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

    let mut child_env = try!(Environment::bind(args_to_fulfill, args, env));

    func.eval(&mut child_env)
}
