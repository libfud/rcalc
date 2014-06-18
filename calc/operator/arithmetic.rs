//!Basic arithemtic functions. 

extern crate num;

use std::num;
use super::unbox_it;
use super::super::{CalcResult, Environment, Evaluate};
use super::super::literal::{Symbol, BigNum, Void};
use self::num::rational::BigRational;

pub type Args<T = Box<Evaluate>> = Vec<T>;
pub type BigR = BigRational;
type Env = Environment;

///Performs addition, subtraction, and multiplication on BigNums.
pub fn do_op(args: &Args, env: &mut Env, min_len: uint, op: |BigR, &BigR| -> BigR,
             ident_fn: || -> BigR) -> CalcResult {
    if args.len() < min_len {
        return Err(" Specified operation requires at least ".to_str().append(
                    min_len.to_str().as_slice()).append( " arguments!"))
    }

    //args is a vector of boxed expressions, which need to be evaluated. Unbox_it
    //handles this for us, and returns a vector of literaltypes. Variables and
    //functions return BigNums, Booleans.
    let literals = try!(unbox_it(args, env));
    let ident: BigR = ident_fn();

    //Find out the contents of the vector.
    let mut stripped_literals: Vec<BigR> = Vec::new();
    for lit in literals.move_iter() {
        match lit {
            BigNum(x) => stripped_literals.push(x),
            Symbol(x) => stripped_literals.push(match try!(env.lookup(&x)) {
                BigNum(y) => y.clone(),
                Void => ident.clone(),
                _ => return Err(try!(env.lookup(&x)).to_str()),
            }),
            Void => { },
            _ => {
                return Err("Arithmetic only works for numbers!".to_str())
            }
        }
    };

    if args.len() == 0 {
        Ok(BigNum(ident))
    }
    else if args.len() == 1 {
        //(+ 1) -> 1, (+ -2) -> -2, (- 3) -> -3, (- -4) -> 4
        Ok(BigNum(stripped_literals.iter().fold(ident, op)))
    } else {
        let first = stripped_literals.as_slice()[0].clone();
        let tail = stripped_literals.slice_from(1);
        //(- 2 3) -> -1
        Ok(BigNum(tail.iter().fold(first, op)))
    }
}


///Divides a vector of bignums. Takes a reference to boxed values for arguments,
///and a reference to the environment, and returns a result which is either
///Ok(LiteralType) or Err(String).
pub fn divrem(args: &Args, env: &mut Env, op:|BigR, &BigR| -> BigR) -> CalcResult {
    if args.len() < 1 {
        return Err("Division requires at least one argument!".to_str())
    }

    let literals = try!(unbox_it(args, env));

    let one: BigR = num::one();

    let mut stripped_literals: Vec<BigR> = Vec::new();
    for lit in literals.move_iter() {
        match lit {
            BigNum(x) => stripped_literals.push(x),
            Symbol(x) => stripped_literals.push(match try!(env.lookup(&x)) {
                BigNum(y) => y.clone(),
                Void => one.clone(),
                _ => return Err(try!(env.lookup(&x)).to_str()),
            }),
            Void => {            }
            _ => {
                return Err("Arithmetic only works for numbers!".to_str())
            }
        }
    };

    let one: BigRational = num::one();

    if args.len() == 1 {
        if *stripped_literals.get(0) == num::zero() {
            return Err("Division by zero is not allowed!".to_str())
        }
        return Ok(BigNum(op(one, stripped_literals.get(0))))
    }

    let first = stripped_literals.as_slice()[0].clone();
    let tail = stripped_literals.slice_from(1);
    let answer = try!(tail.iter().fold(Ok(first), |quot, x|
        quot.and_then(|q| if *x == num::zero() {
                Err(("Division by zero is not allowed!".to_str()))
            } else {
                Ok(op(q, x))
            }
        )
    ));
    Ok(BigNum(answer))
}
