//!Basic arithemtic functions. 

extern crate num;

use std::num;
use super::{big_bool_matrix, unbox_it};
use super::super::literal::{Matrix, BigNum};
use super::super::{CalcResult, Environment, Evaluate};
use self::num::rational::BigRational;

///Performs addition, multiplication, subtraction and division on matrices
pub fn matrix_op<T: Clone>(terms: Vec<Vec<T>>, op: |T, &T| -> T, ident: T) -> Vec<T> {

    let mut acc_vec: Vec<T> = Vec::new();
    let matrix_len = terms.as_slice()[0].len();

    for i in range(0u, matrix_len) {
        let column: Vec<T> = terms.iter().fold(vec![], |mut acc, elem| {
            acc.push(elem.as_slice()[i].clone());
            acc
        });

        match terms.len() {
            1   => {
                acc_vec.push(column.iter().fold(ident.clone(), |acc, x| op(acc, x)));
            },
            _   => {
                let head = column.as_slice()[0].clone();
                let tail = column.slice_from(1);
                acc_vec.push(tail.iter().fold(head, |acc, x| op(acc, x)));
            }
        }
    }

    acc_vec
}

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
///Performs addition, subtraction, and multiplication on BigNums and Matrices.
///Takes the following:
///A reference to a vector of arguments, which are a vector of LiteralTypes.
///LiteralTypes can be BigRationals, matrices, and a few other types not relevant to this.
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
    //functions return BigNums, Booleans and Matrices.
    let literals = try!(unbox_it(args, env));
    let ident: BigRational = ident_fn();

    //Find out the contents of the vector.
    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => Ok(BigNum(ident_fn())), //this is incorrect for (- ), unfortunately

        (_    , _    ,  true)   => Err("Attempted nonsense boolean operation!".to_str()),

        (true ,  true, false)   => Err("Attempted mixed operation!".to_str()),

        (true , false, false)   => {
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
        },

        //It's a vector holding matrices. Strip and pass to matrix_op
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();
            Ok(Matrix(matrix_op(stripped_m, op, ident)))
        }
    }
}


///Divides bignums and matrices. Takes a reference to boxed values for arguments,
///and a reference to the environment, and returns a result which is either
///Ok(LiteralType) or Err(String).
pub fn div(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() < 1 {
        return Err("Division requires at least one argument!".to_str())
    }

    let literals = try!(unbox_it(args, env));
    let one: BigRational = num::one();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => fail!("Impossible condition!"), //see first test in this fn

        (_    , _    ,  true)   => Err("Attempted boolean division!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed division!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();

            if args.len() == 1 {
                if *stripped_literals.get(0) == num::zero() {
                    return Err("Division by zero is not allowed!".to_str())
                }
                Ok(BigNum(one / *stripped_literals.get(0)))
            } else {
                let first = stripped_literals.as_slice()[0].clone();
                let tail = stripped_literals.slice_from(1);
                let answer = try!(tail.iter().fold(Ok(first), |quot, x|
                    quot.and_then(|q| 
                        if *x == num::zero() {
                            Err(("Division by zero is not allowed!".to_str()))
                        } else {
                            Ok(q / *x)
                        }
                    )
                ));
                Ok(BigNum(answer))
            }
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();
            for v in stripped_m.iter() {
                if v.iter().any(|x| *x == num::zero()) {
                    return Err("Division by zero is not allowed!".to_str())
                }
            }

            Ok(Matrix(matrix_op(stripped_m, |a, b| a / *b, one)))
        }
    }
}
