//! Evaluate functions defined by the user

use super::{CalcResult, Environment, Atom, SExpr, arg_to_literal};
use super::literal::{Proc, Boolean, Symbol};
use super::operator;
use super::operator::If;
use super::expression::{Operator, ArgType};

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &mut Environment) -> CalcResult<(ArgType, Environment)> {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, mut func) = match value {
        Proc(x, y) => (x, y),
        _ => return Ok((Atom(value), env.clone()))
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
        child_env.symbols.insert(key.clone(), val);
    }

    loop {
        if func.expr_type != Operator(operator::If) {
            return Ok((SExpr(func), child_env))
//            return func.eval(&mut child_env)
        } else {
            if func.args.len() != 3 {
                return Err("`if` requires three arguments".to_str())
            }
            
            let condition = match func.args.get(0) {
                &Atom(ref x) => match x {
                    &Boolean(val) => val,
                    _ => return Err("Only boolean expressions can be a condition!".to_str()),
                },
                &SExpr(ref x) => match try!(x.eval(&mut child_env)) {
                    Atom(Boolean(val)) => val,
                    _ => return Err("Only booleans can be conditions!".to_str())
                }
            };
            
            let result = if condition {
                func.args.get(1).clone()
            } else {
                func.args.get(2).clone()
            };

            match result {
                Atom(_) => return Ok((func.args.get(2).clone(), child_env)),
                SExpr(ref x) => {
                    if x.expr_type == Operator(operator::If) {
                        func.expr_type = x.expr_type.clone();
                        func.args = x.args.clone();
                    } else {
                        return Ok((SExpr(x.clone()), child_env))
                    }
                }
            }
        }
    }
}
