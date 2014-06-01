//!Basic arithemtic functions. 

extern crate num;

use std::num;
use super::{has_bigs_or_bools, unbox_it};
use super::super::{CalcResult, Environment, Evaluate};
use super::super::literal::BigNum;
use self::num::rational::BigRational;

macro_rules! strip (
    ($x: expr, $case: ident) => {
        $x.map(|y| match y { 
            $case(n) => n,
            _   => unreachable!()
        })
    }
)

pub type Arguments<T = Box<Evaluate>> = Vec<T>;
pub type BigRat = BigRational;
///Performs addition, subtraction, and multiplication on BigNums.
///Takes the following:
///A reference to a vector of arguments, which are a vector of LiteralTypes.
///LiteralTypes can be BigRationals, and a few other types not relevant to this.
///A reference to the environment, which is a struct holding two hashmaps:
///one for variables, and one for functions.
///A minimum length, which is a uint. + and * take >= 0 arguments, - takes >= 1.
///A function to apply, which is either addition, subtraction or multiplication.
///An identity function, which returns either the additive or multiplicative identities.
pub fn do_op(args: &Arguments, env: &mut Environment, min_len: uint,
            op: |BigRat, &BigRat| -> BigRat, ident_fn: || -> BigRat ) -> CalcResult {
    if args.len() < min_len {
        return Err(" Specified operation requires at least ".to_str().append(
                    min_len.to_str().as_slice()).append( " arguments!"))
    }

    //args is a vector of boxed expressions, which need to be evaluated. Unbox_it
    //handles this for us, and returns a vector of literaltypes. Variables and
    //functions return BigNums, Booleans.
    let literals = try!(unbox_it(args, env));
    let ident: BigRational = ident_fn();

    //Find out the contents of the vector.
    let (_, bool_flag)  = has_bigs_or_bools(&literals);
    if bool_flag == true {
        return Err("Attempted nonsense boolean operation!".to_str())
    }

    //it's a vector holding only bigrationals; we're removing the tag from them, so
    //we can work directly with the values
    let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();
        
    if args.len() == 1 {
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
pub fn div(args: &Arguments, env: &mut Environment) -> CalcResult {
    if args.len() < 1 {
        return Err("Division requires at least one argument!".to_str())
    }

    let literals = try!(unbox_it(args, env));
    let one: BigRational = num::one();

    let (_, bool_flag) = has_bigs_or_bools(&literals);
    if bool_flag == true {
        return Err("Attempted nonsensical boolean operation!".to_str())
    }
    let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();

    if args.len() == 1 {
        if *stripped_literals.get(0) == num::zero() {
            return Err("Division by zero is not allowed!".to_str())
        }
        return Ok(BigNum(one / *stripped_literals.get(0)))
    }

    let first = stripped_literals.as_slice()[0].clone();
    let tail = stripped_literals.slice_from(1);
    let answer = try!(tail.iter().fold(Ok(first), |quot, x|
        quot.and_then(|q| if *x == num::zero() {
                Err(("Division by zero is not allowed!".to_str()))
            } else {
                Ok(q / *x)
            }
        )
    ));
    Ok(BigNum(answer))
}
