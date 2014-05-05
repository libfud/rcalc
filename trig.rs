//! Trigonometric functions. 

pub static ONE_ARG_ONLY : &'static str = "This function only takes one argument!";
pub static PI : f64 = 3.141592653589793;

/// Rad converts degrees to radians. Its use is not recommended and it is
/// preferred for other functions to use radsians in the first place. In fact,
/// all other trigonometric functions assume that radians are being used, so
/// if the user wants to use degrees, he will have to use the rad function
/// to convert.
pub fn rad(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let radians = terms[0] * PI / 180.0;

    radians.to_str().to_owned()
}

/// The sin function. Takes either zero or one terms. For no terms,
/// 0 is returned.
pub fn sin(terms: &[f64]) -> ~str {
    if terms.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    if terms.len() == 0 { return "0".to_owned() }
    let answer = terms[0].sin();

    answer.to_str().to_owned()
}

/// The cos function. Takes either zero or one terms. For no terms, 1 is 
/// returned.
pub fn cos(terms: &[f64]) -> ~str {
    if terms.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    if terms.len() == 0 { return "0".to_owned() }
    let answer = terms[0].cos();

    answer.to_str().to_owned()
}

/// The tan function. Takes exactly one argument.
pub fn tan(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let answer = terms[0].tan();

    answer.to_str().to_owned()
}
