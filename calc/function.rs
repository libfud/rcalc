//! Evaluate functions defined by the user

use super::{CalcResult, Environment, ArgType, Atom, SExpr, arg_to_literal};
use super::literal::{Boolean, Proc, Symbol};
use super::operator;
use super::expression::Operator;

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<ArgType>,
            env: &mut Environment) -> CalcResult {
    
    let value = try!(env.lookup(fn_name));

    let (args_to_fulfill, mut func) = match value {
        Proc(x, y) => (x, y),
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
        child_env.symbols.insert(key.clone(), val);
    }

    loop {
        if func.expr_type != Operator(operator::If) {
            return func.eval(&mut child_env)
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
            
            if condition {
                match func.args.get(1).clone() {
                    Atom(_) => return Ok(func.args.get(1).clone()),
                    SExpr(x) => {
                        if x.expr_type == Operator(operator::If) {
                            func.expr_type = x.expr_type.clone();
                            func.args = x.args.clone();
                        } else {
                            return x.eval(&mut child_env)
                        }
                    }
                }
            } else {
                match func.args.get(2).clone() {
                    Atom(_) => return Ok(func.args.get(2).clone()),
                    SExpr(ref x) => {
                        if x.expr_type == Operator(operator::If) {
                            func.expr_type = x.expr_type.clone();
                            func.args = x.args.clone();
                        } else {
                            return x.eval(&mut child_env)
                        }
                    }
                }
            }
        }
    }
}
