//! Trigonometric functions. 

use common::{ONE_ARG_ONLY, str_to_f64};
pub mod common;

pub static PI : f64 = 3.14159265358979323846264338327950288_f64;

/// Rad converts degrees to radians. Its use is not recommended and it is
/// preferred for other functions to use radsians in the first place. In fact,
/// all other trigonometric functions assume that radians are being used, so
/// if the user wants to use degrees, he will have to use the rad function
/// to convert.
pub fn rad(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let result = str_to_f64(terms_str);
    match result {
        Ok(terms) => {
            let radians = terms[0] * PI / 180.0;
            radians.to_str().to_owned()
        }
        Err(msg) => { msg.to_owned() }
    }
}

pub fn deg(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let result = str_to_f64(terms_str);
    match result {
        Ok(terms) => {
            let degrees = terms[0] * 180.0 / PI;
            degrees.to_str().to_owned()
        }
        Err(msg) => { msg.to_owned() }
    }
}

/// The sin function. Takes either zero or one terms. For no terms,
/// 0 is returned.
pub fn sin(terms_str: &[~str]) -> ~str {
    if terms_str.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    let result = str_to_f64(terms_str);
    match result {
        Ok(terms) => {
            if terms.len() == 0 { return "0".to_owned() }
            let answer = terms[0].sin();

            answer.to_str().to_owned()
        }
        Err(msg) => { msg.to_owned() }
    }
}

/// The cos function. Takes either zero or one terms. For no terms, 1 is 
/// returned.
pub fn cos(terms_str: &[~str]) -> ~str {
    if terms_str.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    let result = str_to_f64(terms_str);
    match result {
        Ok(terms) => {
            if terms.len() == 0 { return "0".to_owned() }
            let answer = terms[0].cos();

            answer.to_str().to_owned()
        }
        Err(msg) => { msg.to_owned() }
    }
}

/// The tan function. Takes exactly one argument.
pub fn tan(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let result = str_to_f64(terms_str);
    match result {
        Ok(terms) => {
            let answer = terms[0].tan();
            answer.to_str().to_owned()
        }
        Err(msg) => { msg.to_owned() }
    }
    
}
