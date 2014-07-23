//!Pretty print just prints the "relevant" information for a result.

use super::types::literal::{LiteralType, Symbol, Void};
use super::{Environment, Evaluate, CalcResult};
use std::rc::Rc;
use std::cell::RefCell;

pub fn pretty_print(result: &CalcResult, env: &Rc<Environment>) -> String {
    let res = match result {
        &Ok(ref v) => v.clone(),
        &Err(ref m) => return m.to_string()
    };

    let success = match res.arg_to_literal(&mut env.clone()) {
        Ok(v) => v.clone(),
        Err(m) => return m.to_string()
    };

    if success == Void {
        return "".to_string()
    }

    pretty(&success, env)
}

pub fn pretty(arg: &LiteralType, env: &Rc<Environment>) -> String {
    let s = match arg {
        &Symbol(ref s) => match env.lookup(s) {
            Ok(x) => pretty(x, env),
            Err(_) => s.to_string()
        },
        &Void => "".to_string(),
        x => x.to_string()
    };
    s.append(" ")
}
            
