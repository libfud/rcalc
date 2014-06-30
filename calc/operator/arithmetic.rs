//!Basic arithemtic functions. 

use std::num;
use super::super::{CalcResult, Environment, BadArgType, BadNumberOfArgs, DivByZero};
use super::super::literal::{BigNum, Void};
use super::{BigRational, ArgType, Atom};

pub type Args<T = ArgType> = Vec<T>;
pub type BigR = BigRational;
pub type Env = Environment;

///Performs addition, subtraction, and multiplication on BigNums.
pub fn do_op(args: &Args, env: &mut Env, min_len: uint, op: |BigR, &BigR| -> BigR,
             ident_fn: || -> BigR) -> CalcResult {
    if args.len() < min_len {
        return Err(BadNumberOfArgs(format!(
            " Specified operation requires at least {} arguments", min_len)))
    }

    let ident: BigR = ident_fn();

    //Find out the contents of the vector.
    let mut stripped_literals: Vec<BigR> = Vec::new();
    for arg in args.iter() {
        match try!(arg.desymbolize(env)) {
            BigNum(x) => stripped_literals.push(x),
            Void => { },
            x => return Err(
                BadArgType(format!("Got {} ; arithmetic only works for numbers", x)))
        }
    };

    if args.len() == 0 {
        Ok(Atom(BigNum(ident)))
    } else if args.len() == 1 {
        //(+ 1) -> 1, (+ -2) -> -2, (- 3) -> -3, (- -4) -> 4
        Ok(Atom(BigNum(stripped_literals.iter().fold(ident, op))))
    } else {
        let first = stripped_literals.as_slice()[0].clone();
        let tail = stripped_literals.slice_from(1);
        //(- 2 3) -> -1
        Ok(Atom(BigNum(tail.iter().fold(first, op))))
    }
}


///Divides a vector of bignums. Takes a reference to boxed values for arguments,
///and a reference to the environment, and returns a result which is either
///Ok(LiteralType) or Err(String).
pub fn divrem(args: &Args, env: &mut Env, op:|BigR, &BigR| -> BigR) -> CalcResult {
    if args.len() < 1 {
        return Err(BadNumberOfArgs("Division requires at least one argument!".to_str()))
    }

    let one: BigR = num::one();

    let mut stripped_literals: Vec<BigR> = Vec::new();
    for arg in args.iter() {
        match try!(arg.desymbolize(env)) {
            BigNum(x) => stripped_literals.push(x),
            Void => { },
            _ => return Err(BadArgType("Arithmetic only works for numbers!".to_str()))
        }
    };

    if args.len() == 1 {
        if *stripped_literals.get(0) == num::zero() {
            return Err(DivByZero)
        }
        return Ok(Atom(BigNum(op(one, stripped_literals.get(0)))))
    }

    let first = stripped_literals.as_slice()[0].clone();
    let tail = stripped_literals.slice_from(1);
    let answer = try!(tail.iter().fold(Ok(first), |quot, x|
        quot.and_then(|q| if *x == num::zero() {
                Err(DivByZero)
            } else {
                Ok(op(q, x))
            }
        )
    ));
    Ok(Atom(BigNum(answer)))
}
