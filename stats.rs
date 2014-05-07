//! Statistical functions

extern crate num;

use self::num::rational::{BigRational, Ratio};
use self::num::bigint::ToBigInt;
use arithmetic::add;
use common::{DESPAIR, str_to_rational};

pub mod arithmetic;

/// Avg function returns the arithmetic mean of the terms.
pub fn avg(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 { 
        return "This function requires at least one term.".to_owned()
    }

    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let length: BigRational = Ratio::new(
        (terms.len() as int).to_bigint().unwrap(), 1i.to_bigint().unwrap());
    let average = match from_str::<BigRational>(add(terms_str)) {
        Some(num)   => num.div(&length).to_str().to_owned(),
        _           => DESPAIR.to_owned()
    };

    average
}
