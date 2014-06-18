//!Pretty print just prints the "relevant" information for a result.

use super::literal::trans_literal;
use super::{Environment, CalcResult};

pub fn pretty_print(result: &CalcResult, env: &Environment) {
    let success = match result {
        &Ok(ref v)  => v.clone(),
        &Err(ref m) => {
            println!("{}", m);
            return
        }
    };

    println!("{}", match trans_literal(success, &mut env.clone()) {
        Ok(x) => x.to_symbol(&mut env.clone()),
        Err(m) => m,
    });
}
