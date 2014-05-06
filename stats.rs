//! Statistical functions

use arithmetic::add;
use common::{DESPAIR, str_to_f64};

pub mod arithmetic;

/// Avg function returns the arithmetic mean of the terms.
pub fn avg(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 { 
        return "This function requires at least one term.".to_owned()
    }
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
    let average = match from_str::<f64>(add(terms_str)) {
        Some(num)   => (num / terms.len() as f64).to_str(),
        _           => DESPAIR.to_owned()
    };

    average
}
