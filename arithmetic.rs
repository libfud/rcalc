//! Arithmetic functions.

use common::{DESPAIR, str_to_f64};
pub mod common;

pub static BAD_EXPR : &'static str = "Poorly formatted expression!";
pub static DIV_BY_ZERO : &'static str = "Division by zero is undefined";
pub static ONE_ARG_ONLY : &'static str = 
    "This function only takes one argument!";

/// Adds the numbers in a vector. If there are zero terms, it returns 0.
pub fn add(terms_str: &[~str]) -> ~str {
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
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
pub fn sub(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Subtraction requires at least one term!");
        return BAD_EXPR.to_owned()
    } 
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
    if terms.len() == 1 {
        let difference = 0.0 - terms[0];
        //negative val of first term
        return difference.to_str().to_owned()
    };
    let mut difference = terms[0];
    for term in terms.slice_from(1).iter(){ difference -= *term }

    difference.to_str().to_owned()
}

/// Multiplies the numbers in a vector. Returns 1 for no terms. Otherwise
/// it returns the product of all numbers in a vector.
pub fn mul(terms_str: &[~str]) -> ~str {
    let mut product = 1f64;
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
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
pub fn div(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Division requires at least one term!");
        return BAD_EXPR.to_owned()
    }
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
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
pub fn rem(terms_str: &[~str]) -> ~str {
    if terms_str.len() < 1 {
        println!("Modulus operations require at least two terms!");
        return BAD_EXPR.to_owned()
    }
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
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

/// Pow raises a number to a power - if there are more than one terms,
/// it behaves like a tower of power. It uses the identity function
/// for no terms and for the case of (pow 0 0). In the case of (pow 0 0 0),
/// this will actually evaluate to 0, and (pow 0 0 0 0) will evaluate
/// to one again. This behavior is periodic. Towers are evaluated recursively.
/// If only one number is passed, the number is returned, unless it is zero,
/// which returns zero.
pub fn pow(terms_str: &[~str]) -> ~str {
    if terms_str.len() == 0 { return "1".to_owned() }
    let (message, terms) = str_to_f64(terms_str);
    if message != "OK!" { return message.to_owned() }
    if terms.len() == 1 { 
        if terms.as_slice()[0] != 0.0 { 
            return terms[0].to_str().to_owned()
        }
        else { return "0".to_owned() }
    }
    let base = terms[0];
    let mut exponent : f64;
    if terms.len() == 2 {
        exponent = terms.as_slice()[1];
    } else {
        let temp_exponent = pow(terms_str.slice_from(1));
        match from_str::<f64>(temp_exponent) {
            Some(good_value)    => { exponent = good_value },
            _                   => { return DESPAIR.to_owned() }
        }
    }
    if base == 0.0 && exponent == 0.0 { return "1".to_owned() }
    else if base == 0.0 { return "0".to_owned() }

    let mut rootx = 1.0;
  
    let mut recip_flag = false;
    if exponent < 0.0 { 
        recip_flag = true;
        exponent = exponent.abs();
    }

    let index = exponent - exponent.floor();
    if index > 0.0 {
        match from_str::<f64>(root_wrapper(&[base, index.recip()])) {
            Some(num)   => { rootx = num },
            _           => { return DESPAIR.to_owned() }
        }
    }

    let mut product = 1f64;
    for _ in range(0, exponent.floor() as int) { product *= base; }
    product *= rootx;

    if recip_flag == true { product = 1.0 / product }

    product.to_str().to_owned()
}

/// Root finds a number which when raised to a power equal to the index is
/// equal to the radicand. It requires two arguments: the index and a
/// radicand. 
pub fn root_wrapper(terms: &[f64]) -> ~str {
    if terms.len() != 2 { 
        return "A radicand and index, only, are required.".to_owned()
    }
    let (radicand, index) = (terms[0], terms[1]);

    if index == 0.0 { return "1".to_owned() } //handles (root 0 0)
    if radicand == 0.0 { return "0".to_owned() }
    if index % 2.0 == 0.0 && radicand < 0.0 {
        return "I can't handle this complexity!".to_owned()
    }

    let mut denominator = 0.0;
    if index.floor() < index {
        match index.recip() <= 0.5 {
            true    => { denominator = index - index.floor() },
            false   => { denominator = index.recip() - 0.5 }
        }
    }

    let dummycheck = index.recip();
    match dummycheck <= 0.5 {
        true    => { },
        false   => { denominator -= 0.5 }
    }

    let factor = match denominator {
        0.0 => { 1.0 }
        _   => { radicand.powf(denominator) }
    };
    //this is lazy but I can't suss out how to prevent infinite
    //recursion on fractions > 1/2
    let numerator = index.floor();
    let guess = 1.0;
    let root_of_radicand = root(guess, radicand, numerator);
    if root_of_radicand == -0.0 { return DESPAIR.to_owned() }

    let answer = root_of_radicand * factor;

    answer.to_str().to_owned()
}

/// This is the means to which the root function can attain recursion.
/// Compares the absolute value of the difference of the guess raised to the
/// power and the radicand to a tolerance. If it's within tolerance, that
/// number is returned. Otherwise, it uses the average
pub fn root(guess: f64, radicand: f64, index: f64)  -> f64 {
    let tolerance = match index { //aka epsilon
        2.0 => 0.00001,
        _   => 0.00001,
    };
    let mut guess_to_pow: f64;
    match from_str::<f64>(pow(&[guess.to_str(), index.to_str()])) {
        Some(num)   => { guess_to_pow = num },
        _           => { return -0.0 }
    }
    if (guess_to_pow - radicand).abs() < tolerance {
        return guess
    }
    let mut new_guess: f64;
    match index {
        2.0 => { new_guess = (guess + radicand / guess) / 2.0 }
        _   => { 
            let delta = index.recip() * ((
                radicand /
                (from_str::<f64>(pow(&[guess.to_str(), (index - 1.0).to_str()])
                ).unwrap()) - guess));
            new_guess = guess + delta
        }
    }

    root(new_guess, radicand, index)
}
