//! Evaluate functions defined by the user

use std::rc::Rc;
use super::{CalcResult, Environment, Evaluate, ArgType, Atom, Proc, BadNumberOfArgs};

///Returns the value of the function for the arguments given
#[inline]
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &Rc<Environment>) -> CalcResult {
    
    let (args_to_fulfill, func) = match try!(env.lookup(fn_name)) {
        &Proc(ref x, ref y) => (x.clone(), y.clone()),
        x => return Ok(Atom(x.clone()))
    };

    if args.len() != args_to_fulfill.len() {
        return Err(BadNumberOfArgs(fn_name.clone(), "only".to_string(), args_to_fulfill.len()))
    }

    let mut child_env = Environment::new_frame(env.clone());
    for (arg, val) in args_to_fulfill.iter().zip(args.iter()) {
        child_env.symbols.insert(arg.clone(), try!(val.arg_to_literal(env)));
    }

    func.eval(&mut child_env)
}
