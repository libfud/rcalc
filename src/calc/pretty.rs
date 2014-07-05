//!Pretty print just prints the "relevant" information for a result.


use std::num;
use super::types::literal::*;
use super::types::sexpr::{Function, BuiltIn};
use super::{Atom, SExpr, Environment, Evaluate, CalcResult};

pub fn pretty_print(result: &CalcResult, env: &Environment) -> String {
    let res = match result {
        &Ok(ref v) => v.clone(),
        &Err(ref m) => return m.clone().to_symbol()
    };

    let success = match res.arg_to_literal(&mut env.clone()) {
        Ok(v) => v.clone(),
        Err(m) => return m.to_symbol()
    };

    if success == Void {
        return "".to_str()
    }

    pretty(&success, env)
}

pub fn pretty(arg: &LiteralType, env: &Environment) -> String {
    let s = match arg {
        &BigNum(ref x) => if x.denom() == &num::one() {
            x.numer().to_str()
        } else {
            x.to_str()
        },
        &Boolean(b) => b.to_str(),
        &List(ref l) => {
            let mut list = "(".to_str();
            for elem in l.iter() {
                list = list.append(pretty(elem, env).as_slice());
            }
            list = list.append(")");
            list
        },
        &Matrix(ref x) => x.to_str(),
        &Proc(ref args, ref body) => {
            let mut symbols = args.to_str();
            symbols = symbols.append(" (");
            match body.expr_type {
                Function(ref f) => {
                    symbols = symbols.append(f.as_slice());
                }
                BuiltIn(ref op) => {
                    symbols = symbols.append(op.op_to_str().as_slice());
                }
            }
            for argument in body.args.iter() {
                let arg = match argument {
                    &Atom(ref x) => pretty(x, env).to_str(),
                    &SExpr(ref x) => x.to_str()
                };
                symbols = symbols.append(arg.as_slice());
            }
            symbols = symbols.append(")");
            symbols
        },
        &Symbol(ref s) => match env.lookup(s) {
            Ok(x) => pretty(&x, env),
            Err(_) => s.to_str()
        },
        &Void => "".to_str()
    };
    s.append(" ")
}
            
