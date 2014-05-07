//! Logical functions, including ordering.

use common::{str_to_f64, TWO_ARGS_ERR, DESPAIR};

pub mod common;

/// A function for ordering
pub fn order(comparator: &str, terms: &[~str]) -> ~str {
    if terms.len() != 2 { return TWO_ARGS_ERR.to_owned() }
    let result = str_to_f64(terms);
    if result.is_err() { return result.unwrap_err().to_owned() }
    let comparators = result.ok().unwrap();
    let (lcomp, rcomp) = (comparators[0], comparators[1]);

    match comparator {
        "<"     => { return (lcomp < rcomp).to_str().to_owned() },
        "<="    => { return (lcomp <= rcomp).to_str().to_owned() },
        "="     => { return (lcomp == rcomp).to_str().to_owned() },
        ">="    => { return (lcomp >= rcomp).to_str().to_owned() },
        ">"     => { return (lcomp > rcomp).to_str().to_owned() }
        _       => { return DESPAIR.to_owned() }
    }
}
