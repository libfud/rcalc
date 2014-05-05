//! Statistical functions

use arithmetic::add;

pub static DESPAIR : &'static str = "Laundry day is a very dangerous day.";

pub mod arithmetic;

/// Avg function returns the arithmetic mean of the terms.
pub fn avg(terms: &[f64]) -> ~str {
    if terms.len() < 1 { 
        return "This function requires at least one term.".to_owned()
    }
    let average = match from_str::<f64>(add(terms)) {
        Some(num)   => (num / terms.len() as f64).to_str(),
        _           => DESPAIR.to_owned()
    };

    average
}
