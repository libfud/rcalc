//! Arithmetic functions.

extern crate num;

use super::{Evaluate, CalcResult};
use self::num::rational::BigRational;
use std::num;
use common::{DESPAIR, str_to_rational};
pub mod common;

pub static BAD_EXPR : &'static str = "Poorly formatted expression!";
pub static DIV_BY_ZERO : &'static str = "Division by zero is undefined";
pub static ONE_ARG_ONLY : &'static str = 
    "This function only takes one argument!";

/// Returns the absolute value of the number.
pub fn abs(terms: &Vec<Box<Evaluate>>) -> CalcResult {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }

    let zero: BigRational = num::zero();
    let mut terms: ~[BigRational];
    
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }

    if terms[0] > zero { return terms[0].to_str().to_owned() }
    
    sub(terms_str)
}

/// Adds the numbers in a vector. If there are zero terms, it returns 0.
pub fn add(terms_str: &[~str]) -> ~str {
    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }

    let  mut total: BigRational = num::zero();
    for term in terms.iter() {
        total = total.add(term);
    }

    total.to_str().to_owned()
}

/// Subtracts the numbers in a vector. At least one term is required. If
/// there is only one term, it returns the negative value of that term.
/// Otherwise, it starts subtracting from the left element. IE, if you
/// have an expression (- 10 3 2), 3 is subtracted from 10, and 2 is
/// subtracted from that value.
pub fn sub(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Subtraction requires at least one term!");
        return BAD_EXPR.to_owned()
    } 

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }

    let zero: BigRational = num::zero();
    if terms.len() == 1 {
        let difference = zero.sub(&terms[0]);
        //negative val of first term
        return difference.to_str().to_owned()
    };

    let mut difference = terms[0].clone();
    for term in terms.slice_from(1).iter() {
        difference = difference.sub(term)
    }

    difference.to_str().to_owned()
}

/// Multiplies the numbers in a vector. Returns 1 for no terms. Otherwise
/// it returns the product of all numbers in a vector.
pub fn mul(terms_str: &[~str]) -> ~str {

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }


    let mut product: BigRational = num::one();
    for term in terms.iter() { 
        product = product.mul(term);
    }
    
    product.to_str().to_owned()
}

/// Divides the numbers in a vector. Requires at least one term. If there is
/// only one term, it returns its inverse. Otherwise, it returns the quotient
/// of the first term by the following terms. For example, (/ 12 2 3) will
/// be evaluated as 12 / 2 (6), divided by 3 ( 6 / 3 = 2)
pub fn div(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Division requires at least one term!");
        return BAD_EXPR.to_owned()
    }
    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }

    let zero: BigRational = num::zero();

    if terms.len() == 1 { 
        match terms[0] == zero {
            true    => { return DIV_BY_ZERO.to_owned() }
            false   => { return terms[0].recip().to_str().to_owned(); }
        }
    }

    let mut quotient = terms[0].clone();
    if quotient == zero { return DIV_BY_ZERO.to_owned() }

    for term in terms.slice_from(1).iter() { 
        match *term == zero {
            true    => { return DIV_BY_ZERO.to_owned() }
            false   => { quotient = quotient.div(term) }
        }
    }

    quotient.to_str().to_owned()
}

/// Returns the remainder from integer division. Casts the terms to integers.
/// Requires at least one term; if there is only one term, 1 is returned.
/// Otherwise, it functions similarly to division and subtraction.
pub fn rem(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Modulus operations require at least two terms!");
        return BAD_EXPR.to_owned()
    }

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array },
        Err(msg)            => { return msg.to_owned() }
    }

    let zero: BigRational = num::zero();
    if terms.len() == 1 { 
        match terms[0] == zero {
            true    => { return DIV_BY_ZERO.to_owned() }
            false   => { return "1".to_owned() } // 1 % anything = 1
        }
    }

    let mut remainder = terms[0].clone();
    if remainder.is_integer() == false { 
        return "Non integer modulus is forbidden!".to_owned()
    }

    for term in terms.slice_from(1).iter() { 
        if term.is_integer() == false {
            return "Non integer modulus is forbidden!".to_owned()
        }
        match *term == zero {
            true    => { return DIV_BY_ZERO.to_owned() },
            false   => { remainder = remainder.rem(term)}
        }
    }

    remainder.to_str().to_owned()
}
