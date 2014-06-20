//!Pretty print just prints the "relevant" information for a result.

use super::{Environment, CalcResult, arg_to_literal};

pub fn pretty_print(result: &CalcResult, env: &Environment) {
    let res = match result {
        &Ok(ref v) => v.clone(),
        &Err(ref m) => {
            println!("{}", m);
            return
        }
    };

    let success = match arg_to_literal(&res, &mut env.clone()) {
        Ok(v)  => v.clone(),
        Err(ref m) => {
            println!("{}", m);
            return
        }
    };

    println!("{}", success);

}
