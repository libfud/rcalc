//! Arithmetic functions.

pub static BAD_EXPR : &'static str = "Poorly formatted expression!";
pub static DIV_BY_ZERO : &'static str = "Division by zero is undefined";
pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";

/// Adds the numbers in a vector. If there are zero terms, it returns 0.
pub fn add(terms: &[f64]) -> ~str {
    let  mut total = 0f64;
    for term in terms.iter() {
        total += *term;
    }

    total.to_str().to_owned()
}

/// Subtracts the numbers in a vector. At least one term is required. If
/// there is only one term, it returns the negative value of that term.
/// Otherwise, it starts subtracting from the left element. IE, if you
/// have an expression (- 10 3 2), 3 is subtracted from 10, and 2 is
/// subtracted from that value.
pub fn sub(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Subtraction requires at least one term!");
        return BAD_EXPR.to_owned()
    } 
    if terms.len() == 1 {
        let difference = 0f64 - terms[0];
        //negative val of first term
        return difference.to_str().to_owned()
    }
    let mut difference = terms[0];
    for term in terms.slice_from(1).iter(){ difference -= *term }

    difference.to_str().to_owned()
}

/// Multiplies the numbers in a vector. Returns 1 for no terms. Otherwise
/// it returns the product of all numbers in a vector.
pub fn mul(terms: &[f64]) -> ~str {
    let mut product = 1f64;
    for term in terms.iter() { 
        match *term {
            0.0 => { return "0".to_owned() }
            _   => { product *= *term } 
        }
    }
    
    product.to_str().to_owned()
}

/// Divides the numbers in a vector. Requires at least one term. If there is
/// only one term, it returns its inverse. Otherwise, it returns the quotient
/// of the first term by the following terms. For example, (/ 12 2 3) will
/// be evaluated as 12 / 2 (6), divided by 3 ( 6 / 3 = 2)
pub fn div(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Division requires at least one term!");
        return BAD_EXPR.to_owned()
    }
    if terms.len() == 1 { 
        match terms[0] {
            0.0  => { return DIV_BY_ZERO.to_owned() }
            _    => { return (1f64 / terms[0]).to_str().to_owned(); }
        }
    }
    let mut quotient = terms[0];
    if quotient == 0.0 || quotient == -0.0 { return DIV_BY_ZERO.to_owned() }
    for term in terms.slice_from(1).iter() { 
        match *term {
            0.0 => { return DIV_BY_ZERO.to_owned() }
            _   => { quotient /= *term }
        }
    }

    quotient.to_str().to_owned()
}

/// Returns the remainder from integer division. Casts the terms to integers.
/// Requires at least one term; if there is only one term, 1 is returned.
/// Otherwise, it functions similarly to division and subtraction.
pub fn rem(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Modulus operations require at least two terms!");
        return BAD_EXPR.to_owned()
    }
    if terms.len() == 1 { 
        match terms[0] {
            0.0 => { return DIV_BY_ZERO.to_owned() }
            _   => { return "1".to_owned() } // 1 % anything = 1
        }
    }
    let mut remainder = terms[0] as int;
    if terms[0] == 0.0 { return "0".to_owned() }
    for term in terms.slice_from(1).iter() { 
        match *term {
            0.0 => { return DIV_BY_ZERO.to_owned() },
            _   => { remainder %= *term as int }
        }
    }

    remainder.to_str().to_owned()
}
