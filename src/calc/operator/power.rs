//! Methods of raising an index to a given power.

use std::num;
use super::super::{CalcResult, Environment, BadArgType, BadPowerRange};
use super::super::literal::BigNum;
use super::{Mpq, ArgType, Atom};

pub fn pow_wrapper(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    let mut args_vec: Vec<Mpq> = Vec::new();
    for arg in args.iter() {
        match try!(arg.arg_to_literal(env)) {
            BigNum(x)   => args_vec.push(x),
            _ => return Err(BadArgType("Only numbers can be raised to a power".to_str()))
        }
    }

    Ok(Atom(BigNum(try!(pow(args_vec.as_slice())))))
}

/// Pow raises a number to a power - if there are more than one terms, it
/// behaves like a tower of power. It uses the identity function for no terms
/// and for the case of (pow 0 0). In the case of (pow 0 0 0), this will
/// actually evaluate to 0, and (pow 0 0 0 0) will evaluate to one again. This
/// behavior is periodic. Towers are evaluated recursively. If only one number
/// is passed, the number is returned, unless it is zero, which returns zero.
pub fn pow(args: &[Mpq]) -> CalcResult<Mpq> {
    if args.len() == 0 {
        return Ok(num::one())
    } else  if args.len() == 1 && args[0] != num::zero() {
        return Ok(args[0].clone())
    } else if args.len() == 1 && args[0] == num::zero() {
        return Ok(num::zero())
    }

    let base = args[0].clone();
    let mut exponent = match args.len() {
        2   => args[1].clone(),
        _   => try!(pow(args.slice_from(1)))
    };

    if base == num::zero() && exponent == num::zero() { 
        return Ok(num::one())
    } else if base == num::zero() {
        return Ok(num::zero())
    }

    //Mpqs need to be cloned due to shallow copying
    let mut rootx: Mpq = num::one();
    let zero: Mpq = num::zero();
    let mut recip_flag = false;

    if exponent < zero { 
        exponent = zero - exponent;
        recip_flag = true;
    }

/*
    let power = match exponent.floor().to_integer().to_u64() {
        Some(x) => x,
        None    => return Err(BadPowerRange)
    };
*/

    let power = 
    let index = exponent - exponent.floor();

    if index > zero {
        rootx = try!(root_wrapper(base.clone(), index.invert()));
    }

    let product = exp_by_sq(base, power);

    if recip_flag == true {
        Ok((product * rootx).invert())
    } else { 
        Ok(product * rootx)
    }
}

pub fn exp_by_sq(base: Mpq, power: u64) -> Mpq {
    if power == 0 {
        num::one()
    } else if power == 1 {
        base
    } else if power % 2 == 0 {
        exp_by_sq(base * base, power / 2)
    } else {
        base * exp_by_sq(base * base, (power - 1) / 2)
    }
}

/// Root finds a number which when raised to a power equal to the index is
/// equal to the radicand. It requires two arguments: the index and a
/// radicand. 
pub fn root_wrapper(radicand: Mpq, index: Mpq) -> CalcResult<Mpq> {
    let zero: Mpq = num::zero();
    let one: Mpq = num::one();
    let two = one + one;
    let half = one / two;

    if index == zero {
        return Ok(one)
    } else if radicand == zero { 
        return Ok(zero)
    }

    if index % two == zero && radicand < zero {
        return Err(BadArgType("I can't handle this complexity!".to_str()))
    }

    let mut guess: Mpq = num::one();
    if *index.denom() == num::one() && *radicand.denom() == num::one() {
        match dumb_root(radicand.clone(), index.clone()){
            Ok(good_val)    => return Ok(good_val),
            Err(good_guess) => guess = good_guess,
        }
    }

    let mut denominator = zero.clone();
    if index.get_den() != num::one() {
        match index.invert() <= half {
            true    => denominator = index - (index.get_num() - index.get_den() + num::one())
            false   => denominator = index.invert() - half,
        }
    }

    let dummycheck = index.invert();
    match dummycheck <= half {
        true    => { },
        false   => {
            return Err(BadArgType("me too dum".to_str()))
        }
    }

    let factor: Mpq;
    match denominator == zero {
        true    => { factor = num::one() }
        false   => {
            let inv_denom = denominator.recip();
            factor = try!(pow(&[radicand.clone(), inv_denom]));
        }
    };

    let numerator = index.floor();
    let root_of_radicand = try!(root(guess, radicand, numerator));

    let answer = root_of_radicand.mul(&factor);

    Ok(answer)
}

/// This is the means to which the root function can attain recursion.
/// Compares the absolute value of the difference of the guess raised to the
/// power and the radicand to a tolerance. If it's within tolerance, that
/// number is returned. Otherwise, it uses the average
pub fn root(guess: Mpq, radicand: Mpq, index: Mpq) 
    -> CalcResult<Mpq> {

    let one: Mpq = num::one();
    let two = one + one;
    let tolerance = from_str::<Mpq>("1/10000").unwrap() * radicand;
    let guess_to_pow = try!(pow(&[guess.clone(), index.clone()]));

    let good_enough = match guess_to_pow >= radicand {
        true    => guess_to_pow - radicand,
        false   => radicand - guess_to_pow
    };
    if good_enough < tolerance {
        return Ok(guess)
    }

    let mut new_guess: Mpq;
    match index == two {
        true    => new_guess = (guess + radicand / guess) / two,
        false   => { 
            let i_dont_know = try!(pow(&[guess.clone(), (index.clone() - one)]));
            let delta = index.invert() * (( radicand /(i_dont_know - guess)));
            new_guess = guess + delta
        }
    }

    root(new_guess, radicand, index)
}

/// dumb root method, just like you did in elementary school
pub fn dumb_root(radicand: Mpq, index: Mpq) ->
    Result<Mpq, Mpq> {

    let one: Mpq = num::one();
    let mut guess: Mpq = num::one();

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
