//! Methods of raising an index to a given power.

extern crate num;

use self::num::rational::BigRational;
use std::num;
use self::num::bigint::BigInt;
use super::super::{Evaluate, CalcResult};
use super::super::literal::{BigNum};

pub fn pow_wrapper(args: &Vec<Box<Evaluate>>) -> CalcResult {
    let mut args_vec: Vec<BigRational> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        args_vec.push( match try!(args.get(i).eval()) {
            BigNum(ref x)   => x.clone(),
            _               => {
                return Err("damnit".to_strbuf())
            }
        });
        i += 1;
    }

    Ok(BigNum(try!(pow(args_vec.as_slice()))))
}

/// Pow raises a number to a power - if there are more than one terms, it
/// behaves like a tower of power. It uses the identity function for no terms
/// and for the case of (pow 0 0). In the case of (pow 0 0 0), this will
/// actually evaluate to 0, and (pow 0 0 0 0) will evaluate to one again. This
/// behavior is periodic. Towers are evaluated recursively. If only one number
/// is passed, the number is returned, unless it is zero, which returns zero.
pub fn pow(args: &[BigRational]) -> Result<BigRational, StrBuf> {
    let zero: BigRational = num::zero();
    let one = from_str::<BigRational>("1/1").unwrap(); //ONE

    if args.len() == 0 { return Ok(one) }

    let base = args[0].clone();

    if args.len() == 1 { 
        if base != zero { return Ok(base) }
        else { return Ok(zero) }
    }

    let mut exponent;

    if args.len() == 2 {
        exponent = args[1].clone();
    } else {
        exponent = match pow(args.slice_from(1)) {
            Ok(good_val)    => good_val,
            Err(msg)        => { return Err(msg.to_strbuf()) }
        };
    }

    if base == zero && exponent == zero { return Ok(one) }
    else if base == zero { return Ok(zero) }

    //BigRationals need to be cloned due to shallow copying
    let mut rootx = one.clone();
  
    let mut recip_flag = false;
    if exponent < zero { 
        exponent = zero.sub(&exponent);
        recip_flag = true;
    }

    let index = exponent - exponent.floor();
    if index > zero {
        rootx = match root_wrapper(&[base.clone(), index.recip()]) {
            Ok(value)   => value,
            Err(msg)    => { return Err(msg.to_strbuf()) }
        };
    }

    let mut product = one.clone();
    let mut i = zero.clone();
    loop {
        if i >= exponent.floor() { break }
        product = product.mul(&base);
        i = i.add(&one);
    }

    product = product.mul(&rootx);

    if recip_flag == true { product = product.recip() }

    Ok(product)
}

/// Root finds a number which when raised to a power equal to the index is
/// equal to the radicand. It requires two arguments: the index and a
/// radicand. 
pub fn root_wrapper(terms: &[BigRational]) -> Result<BigRational, StrBuf> {
    if terms.len() != 2 { return Err("A radicand and index are required.".to_strbuf()) }

    let zero = from_str::<BigRational>("0/1").unwrap(); 
    let one = from_str::<BigRational>("1/1").unwrap(); 
    let two = from_str::<BigRational>("2/1").unwrap(); 
    let half = from_str::<BigRational>("1/2").unwrap();

    let (radicand, index) = (terms[0].clone(), terms[1].clone());

    if index == zero { return Ok(one) }
    if radicand == zero { return Ok(zero) }

    if index % two == zero && radicand < zero {
        return Err("I can't handle this complexity!".to_strbuf())
    }

    let mut guess = one.clone();

    let one_bigint: BigInt = num::one();
    if *index.denom() == one_bigint && *radicand.denom() == one_bigint {
        match dumb_root(radicand.clone(), index.clone()){
            Ok(good_val)    => { return Ok(good_val) }
            Err(good_guess) => { guess = good_guess }
        }
    }

    let mut denominator = zero.clone();
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
            println!("Sorry. I'm just too dumb to handle that for now.");
            return Err("me too dum".to_strbuf())
        }
    }

    let factor: BigRational;
    match denominator == zero {
        true    => { factor = one.clone() }
        false   => {
            let inv_denom = denominator.recip();
            factor = try!(pow(&[radicand.clone(), inv_denom.clone()]));
        }
    };

    let numerator = index.floor();
    let root_of_radicand = try!(root(guess.clone(), radicand.clone(), numerator.clone()));

    let answer = root_of_radicand.mul(&factor);

    Ok(answer)
}

/// This is the means to which the root function can attain recursion.
/// Compares the absolute value of the difference of the guess raised to the
/// power and the radicand to a tolerance. If it's within tolerance, that
/// number is returned. Otherwise, it uses the average
pub fn root(guess: BigRational, radicand: BigRational, index: BigRational) 
    -> Result<BigRational, StrBuf> {

    let one: BigRational = num::one();
    let two = one.add(&one);

    let tolerance = from_str::<BigRational>("1/100000").unwrap();

    let guess_to_pow = try!(pow(&[guess.clone(), index.clone()]));

    let good_enough = match guess_to_pow >= radicand {
        true    => guess_to_pow - radicand,
        false   => radicand - guess_to_pow
    };
    if good_enough < tolerance {
        return Ok(guess)
    }

    let mut new_guess: BigRational;
    match index == two {
        true    => { new_guess = (guess + radicand / guess) / two }
        false   => { 
            let i_dont_know = try!(pow(&[guess.clone(), (index.clone() - one)]));
            let delta = index.recip() * (( radicand /(i_dont_know - guess)));
//                (from_str::<BigRational>(pow(&[guess.to_str(), (index - one).to_str()])
//                ).unwrap()) - guess));
            new_guess = guess + delta
        }
    }

    root(new_guess, radicand, index)
}

/// dumb root method, just like you did in elementary school
pub fn dumb_root(radicand: BigRational, index: BigRational) ->
    Result<BigRational, BigRational> {

    let one: BigRational = num::one();
    let mut guess = one.clone();

    loop {
        let mut guess_to_pow;
        match pow(&[guess.clone(), index.clone()]) {
            Ok(bignum)  => { guess_to_pow = bignum }
            Err(_)    => { return Err(guess) }
                //this is okay, because originally guess for newton's method
                //was one anyway
        }
        if guess_to_pow == radicand {
            break
        }
        if guess_to_pow > radicand {
            return Err(guess)
        }
        guess = guess.add(&one);
    }

    Ok(guess)
}
