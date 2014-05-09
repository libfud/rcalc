//! Trigonometric functions. 
extern crate num;

use self::num::rational::BigRational;
use std::num;
use common::{ONE_ARG_ONLY, str_to_rational};
pub mod common;

pub static PI : &'static str = "3126535/995207";
pub static HALF_CIRC: &'static str = "180/1";

/// Rad converts degrees to radians. Its use is not recommended and it is
/// preferred for other functions to use radsians in the first place. In fact,
/// all other trigonometric functions assume that radians are being used, so
/// if the user wants to use degrees, he will have to use the rad function
/// to convert.
pub fn rad(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array }
        Err(msg)            => { return msg.to_owned() }
    }

    let pi = from_str::<BigRational>(PI).unwrap();
    let half_circ = from_str::<BigRational>(HALF_CIRC).unwrap();

    let radians = terms[0].mul(&(pi.div(&half_circ)));

    radians.to_str().to_owned()
}

/// Converst radians to degrees
pub fn deg(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    
    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array }
        Err(msg)            => { return msg.to_owned() }
    }


    let pi = from_str::<BigRational>(PI).unwrap();
    let half_circ = from_str::<BigRational>(HALF_CIRC).unwrap();

    let degrees = terms[0].mul(&(half_circ.div(&pi)));

    degrees.to_str().to_owned()
}

/// The shame function.
pub fn rational_to_f64_trig(bigrational_orig: &BigRational) -> f64 {
    let mut bigrational = bigrational_orig.clone();
    let two = from_str::<BigRational>("2/1").unwrap();
    let zero = num::zero();
    let neg_two = from_str::<BigRational>("-2/1").unwrap();
    let pi = from_str::<BigRational>(PI).unwrap();

    //I feel horrible about the atrocity I'm about to commit.
    match bigrational > zero {
        true    => {
            loop {
                if bigrational < two.mul(&pi) { break }
                bigrational = bigrational.sub(&two.mul(&pi));
            }
        },
        false   => {
            loop {
                if bigrational > neg_two.mul(&pi) { break }
                bigrational = bigrational.add(&two.mul(&pi));
            }
        }
    }

    // please forgive me
    let numer_str = bigrational.numer().to_str();
    let denom_str = bigrational.denom().to_str();

    // oh god
    let numer = from_str::<f64>(numer_str).unwrap();
    let denom = from_str::<f64>(denom_str).unwrap();
    let ration_as_float = numer / denom;

    ration_as_float
}

/// The sin function. Takes either zero or one terms. For no terms,
/// 0 is returned.
pub fn sin(terms_str: &[~str]) -> ~str {
    if terms_str.len() > 1 { return ONE_ARG_ONLY.to_owned() }

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array }
        Err(msg)            => { return msg.to_owned() }
    }
    if terms.len() == 0 { return "0/1".to_owned() }

    let ration_as_float = rational_to_f64_trig(&terms[0]);
    let answer = ration_as_float.sin();

    answer.to_str().to_owned()
}

/// The cos function. Takes either zero or one terms. For no terms, 1 is 
/// returned.
pub fn cos(terms_str: &[~str]) -> ~str {
    if terms_str.len() > 1 { return ONE_ARG_ONLY.to_owned() }

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array }
        Err(msg)            => { return msg.to_owned() }
    }


    if terms.len() == 0 { return "0/1".to_owned() }

    let ration_as_float = rational_to_f64_trig(&terms[0]);
    let answer = ration_as_float.cos();

    answer.to_str().to_owned()
}

/// The tan function. Takes exactly one argument.
pub fn tan(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }

    let mut terms;
    match str_to_rational(terms_str) {
        Ok(bigrat_array)    => { terms = bigrat_array }
        Err(msg)            => { return msg.to_owned() }
    }

    let ration_as_float = rational_to_f64_trig(&terms[0]);
    let sin_numer = ration_as_float.sin();
    let cos_denom = ration_as_float.cos();

    let tangent = sin_numer/cos_denom;

    tangent.to_str().to_owned()
}
