//! Arithmetic functions.

extern crate num;

use self::num::rational::BigRational;
use common::{DESPAIR, str_to_rational};
pub mod common;

pub static BAD_EXPR : &'static str = "Poorly formatted expression!";
pub static DIV_BY_ZERO : &'static str = "Division by zero is undefined";
pub static ONE_ARG_ONLY : &'static str = 
    "This function only takes one argument!";

/// Returns the absolute value of the number.
pub fn abs(terms_str: &[~str]) -> ~str {
    if terms_str.len() != 1 { return ONE_ARG_ONLY.to_owned() }

    let zeero = from_str::<BigRational>("0/1").unwrap();
    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    if terms[0] > zeero { return terms[0].to_str().to_owned() }
    
    sub(terms_str)
}

/// Adds the numbers in a vector. If there are zero terms, it returns 0.
pub fn add(terms_str: &[~str]) -> ~str {
    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let  mut total = from_str::<BigRational>("0/1").unwrap();
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

    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let zeero = from_str::<BigRational>("0/1").unwrap();
    if terms.len() == 1 {
        let difference = zeero.sub(&terms[0]);
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
    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let mut product = from_str::<BigRational>("1/1").unwrap();
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

    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let zeero = from_str::<BigRational>("0/1").unwrap();
    if terms.len() == 1 { 
        match terms[0] == zeero {
            true    => { return DIV_BY_ZERO.to_owned() }
            false   => { return terms[0].recip().to_str().to_owned(); }
        }
    }
    let mut quotient = terms[0].clone();
    if quotient == zeero { return DIV_BY_ZERO.to_owned() }
    for term in terms.slice_from(1).iter() { 
        match *term == zeero {
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

    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }

    let zeero = from_str::<BigRational>("0/1").unwrap();
    if terms.len() == 1 { 
        match terms[0] == zeero {
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
        match *term == zeero {
            true    => { return DIV_BY_ZERO.to_owned() },
            false   => { remainder = remainder.rem(term)}
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
    let zeero = from_str::<BigRational>("0/1").unwrap(); //ZERO
    let wun = from_str::<BigRational>("1/1").unwrap(); //ONE

    if terms_str.len() == 0 { return wun.to_str().to_owned() }

    let (message, terms) = str_to_rational(terms_str);
    if message != "OK!" { return message.to_owned() }


    if terms.len() == 1 { 
        if terms[0] != zeero { 
            return terms[0].to_str().to_owned()
        }
        else { return zeero.to_str().to_owned() }
    }

    let base = terms[0].clone();
    let mut exponent : BigRational;

    if terms.len() == 2 {
        exponent = terms[1].clone();
    } else {
        let temp_exponent = pow(terms_str.slice_from(1));
        match from_str::<BigRational>(temp_exponent) {
            Some(good_value)    => { exponent = good_value },
            _                   => { return DESPAIR.to_owned() }
        }
    }

    if base == zeero && exponent == zeero { 
        return wun.to_str().to_owned()
    } else if base == zeero {
        return zeero.to_str().to_owned()
    }

    let mut rootx = wun.clone();
  
    let mut recip_flag = false;
    if exponent < zeero { 
        recip_flag = true;
        match from_str::<BigRational>(abs(&[exponent.to_str()])) {
            Some(bignum)    => { exponent = bignum },
            _               => { return "fail".to_owned() }
        }
    }

    let index = exponent - exponent.floor();
    if index > zeero {
        let rootx_str = root_wrapper(&[base.clone(), index.recip()]);
        if rootx_str == "fail".to_owned() { return rootx_str }
        match from_str::<BigRational>(rootx_str) {
            Some(num)   => { rootx = num.clone() },
            _           => { return DESPAIR.to_owned() }
        }
    }

    let mut product = wun.clone();
    let mut i = zeero.clone();
    //for _ in range(0, exponent.floor() as int) { product *= base; }
    loop {
        if i >= exponent.floor() { break }
        product = product.mul(&base);
        i = i.add(&wun);
    }

    product = product.mul(&rootx);

    if recip_flag == true { product = product.recip() }

    product.to_str().to_owned()
}

/// Root finds a number which when raised to a power equal to the index is
/// equal to the radicand. It requires two arguments: the index and a
/// radicand. 
pub fn root_wrapper(terms: &[BigRational]) -> ~str {
    let zeero = from_str::<BigRational>("0/1").unwrap(); //ZERO
    let wun = from_str::<BigRational>("1/1").unwrap(); //ONE
    let two = from_str::<BigRational>("2/1").unwrap(); 
    let half = from_str::<BigRational>("1/2").unwrap();

    if terms.len() != 2 { 
        return "A radicand and index, only, are required.".to_owned()
    }

    let (radicand, index) = (terms[0].clone(), terms[1].clone());

    if index == zeero { return "1".to_owned() } //handles (root 0 0)
    if radicand == zeero { return "0".to_owned() }
    if index % two == zeero && radicand < zeero {
        return "I can't handle this complexity!".to_owned()
    }

    let mut denominator = zeero.clone();
    if index.floor() < index {
        match index.recip() <= half {
            true    => { denominator = index.sub(&index.floor()) },
            false   => { denominator = index.recip().sub(&half) }
        }
    }

    let dummycheck = index.recip();
    match dummycheck <= half {
        true    => { },
        false   => {
            println!("Sorry. No fractions > 1/2 for now.");
            return "fail".to_owned();
        }
    }

    let factor: BigRational;
    match denominator == zeero {
        true    => { factor = wun.clone() }
        false   => {
            let inv_denom = denominator.recip();
            let answer_str = pow(&[radicand.to_str(), inv_denom.to_str()]);
            let mut answer: BigRational;
            match from_str::<BigRational>(answer_str) {
                Some(bignum)    => { answer = bignum },
                _               => { return "fail".to_owned() }
            }
            factor = answer
        }
    };

    let numerator = index.floor();
    let guess = wun.clone();
    let root_of_radicand = root(guess, radicand, numerator);

    let answer = root_of_radicand.mul(&factor);

    answer.to_str().to_owned()
}

/// This is the means to which the root function can attain recursion.
/// Compares the absolute value of the difference of the guess raised to the
/// power and the radicand to a tolerance. If it's within tolerance, that
/// number is returned. Otherwise, it uses the average
pub fn root(guess: BigRational, radicand: BigRational, index: BigRational) 
    -> BigRational {

    let zeero = from_str::<BigRational>("0/1").unwrap(); //ZERO
    let wun = from_str::<BigRational>("1/1").unwrap(); //ONE
    let two = from_str::<BigRational>("2/1").unwrap(); 

    let tolerance = from_str::<BigRational>("1/100000").unwrap();
    let mut guess_to_pow: BigRational;

    match from_str::<BigRational>(pow(&[guess.to_str(), index.to_str()])) {
        Some(num)   => { guess_to_pow = num },
        _           => { return zeero}
    }

    let good_enough = match from_str::<BigRational>(
        abs(&[(guess_to_pow - radicand).to_str()])) {
        Some(bignum)    => bignum,
        _               => tolerance.clone()
    };
    if good_enough < tolerance {
        return guess
    }
    let mut new_guess: BigRational;
    match index == two {
        true    => { new_guess = (guess + radicand / guess) / two }
        false   => { 
            let delta = index.recip() * ((
                radicand /
                (from_str::<BigRational>(pow(&[guess.to_str(), (index - wun).to_str()])
                ).unwrap()) - guess));
            new_guess = guess + delta
        }
    }

    root(new_guess, radicand, index)
}
