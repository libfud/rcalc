//! Functions and variables which are used in more than one module.
extern crate num;
use self::num::rational::{BigRational, Ratio};

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
pub static ONE_ARG_ONLY : &'static str =
    "This function only takes one argument!";
pub static TWO_ARGS_ERR : &'static str = "This function only takes two terms!";

pub static E: f64 = 2.71828182845904523536028747135266250_f64;

/// Function to convert an array of owned strings into BigRationals for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_rational(str_array: &[~str]) -> (&str, ~[BigRational]) {
    let mut big_vec: Vec<BigRational> = Vec::new();
    let mut rational: BigRational;

    for elem in str_array.iter() {
        let number_type = get_number_type(*elem);
        if number_type == "invalid representation" {
            return (number_type, big_vec.as_slice().to_owned())
        }
        match number_type {
            "fraction"      => {
                match from_str::<BigRational>(*elem) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
            },

            "non-fraction"  => {
                let mut floated: f64;
                match from_str::<f64>(*elem) {
                    Some(num)   => { floated = num },
                    _           => {
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
                match Ratio::from_float(floated) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
            },

            _               => {
                return (DESPAIR, big_vec.as_slice().to_owned())
            }
        }
        big_vec.push(rational);
    }

    ("OK", big_vec.as_slice().to_owned())
}

/// Determines if a number is a decimal representation or a fractional
/// representation. Mixing is disallowed. Returns a message and the
/// location of the division symbol or radix point.
pub fn get_number_type(num_str: &str) -> &str {
    let bad_sym = "invalid representation";
    let mut div_symbol = false;
    let mut radix_point_symbol = false;

    if num_str.slice_to(1) == "/" ||
        num_str.slice_to(num_str.len() -1) == "/" { return bad_sym  }

    for c in num_str.chars() {
        match c {
            '/' => {
                if div_symbol == true { return bad_sym }
                else { div_symbol = true; }
            },
            '.' => {
                if radix_point_symbol == true { return bad_sym }
                else { radix_point_symbol = true; }
            }
            _   => { } // do nothing, it doesn't concern us
        }
    }

    if div_symbol == true && radix_point_symbol == true {
        bad_sym 
    } else if div_symbol == true {
        "fraction"
    } else {
        "non-fraction"
        // a number without either / or . has an implicit .
    }
}

/// Function to convert an array of owned strings into f64 for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_f64(str_array: &[~str]) -> (&str, ~[f64]) {
    let mut float_vec = Vec::new();
    for elem in str_array.iter() {
        match from_str::<f64>(*elem) {
            Some(num)   => { float_vec.push(num) },
            _           => { return (DESPAIR, [0.0].to_owned()) }
        }
    }

    ("OK!", float_vec.as_slice().to_owned())
}

fn main() {
    println!("test")
}
