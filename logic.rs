//! Logical functions, including ordering.

use common::{str_to_rational, TWO_ARGS_ERR, DESPAIR};

pub mod common;

/// A function for ordering
pub fn order(comparator: &str, terms: &[~str]) -> ~str {
    if terms.len() != 2 { return TWO_ARGS_ERR.to_owned() }

    let (message, comparators) = str_to_rational(terms);
    if message != "OK!" { return message.to_owned() }
    let (lcomp, rcomp) = (comparators[0].clone(), comparators[1].clone());

    match comparator {
        "<"     => { return (lcomp < rcomp).to_str().to_owned() },
        "<="    => { return (lcomp <= rcomp).to_str().to_owned() },
        "="     => { return (lcomp == rcomp).to_str().to_owned() },
        ">="    => { return (lcomp >= rcomp).to_str().to_owned() },
        ">"     => { return (lcomp > rcomp).to_str().to_owned() }
        _       => { return DESPAIR.to_owned() }
    }
}
