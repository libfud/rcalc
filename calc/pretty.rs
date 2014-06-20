//!Pretty print just prints the "relevant" information for a result.

use super::{Environment, CalcResult};

pub fn pretty_print(result: &CalcResult, env: &Environment) {
    let success = match result {
        &Ok(ref v)  => v.clone(),
        &Err(ref m) => {
            println!("{}", m);
            return
        }
    };

    println!("{}", success);
}
