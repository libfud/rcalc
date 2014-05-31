//!Basic arithemtic functions. 

extern crate num;

use std::num;
use super::{big_bool_matrix, unbox_it};
use super::super::literal::{Matrix, BigNum};
use super::super::{CalcResult, Environment, Evaluate};
use self::num::rational::BigRational;

///Performs addition, multiplication, subtraction and division on matrices
pub fn matrix_op(terms: Vec<Vec<BigRational>>, op: |BigRational, &BigRational| -> BigRational,
    ident: BigRational) -> Vec<BigRational> {

    let mut acc_vec: Vec<BigRational> = Vec::new();
    let matrix_len = terms.as_slice()[0].len();

    for i in range(0u, matrix_len) {
        let column: Vec<BigRational> = terms.iter().fold(vec![], |mut acc, elem| {
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

pub fn add(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    let literals = try!(unbox_it(args, env));
    let zero: BigRational = num::zero();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => Ok(BigNum(num::zero())),
        (_    , _    ,  true)   => Err("Attempted boolean addition!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed addition!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();

            Ok(BigNum(stripped_literals.iter().fold(zero, |sum, x| sum.add(x))))
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();
            Ok(Matrix(matrix_op(stripped_m, |a, b| a + *b, zero)))
        }
    }
}

pub fn sub(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() < 1 {
        return Err("Subtraction requires at least one argument".to_str())
    }

    let literals = try!(unbox_it(args, env));
    let zero: BigRational = num::zero();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => fail!("Impossible condition!"), //see first test in this fn
        (_    , _    ,  true)   => Err("Attempted boolean subtraction!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed subtraction!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();

            if args.len() == 1 {
                Ok(BigNum(stripped_literals.iter().fold(zero, |diff, x| diff.sub(x))))
            } else {
                let first = stripped_literals.as_slice()[0].clone();
                let tail = stripped_literals.slice_from(1);
                Ok(BigNum(tail.iter().fold(first, |diff, x| diff.sub(x))))
            }
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();

            Ok(Matrix(matrix_op(stripped_m, |a, b| a - *b, zero)))
        }
    }
}

pub fn mul(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    let literals = try!(unbox_it(args, env));
    let one: BigRational = num::one();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => Ok(BigNum(num::one())),
        (_    , _    ,  true)   => Err("Attempted boolean addition!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed addition!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();
            Ok(BigNum(stripped_literals.iter().fold(one, |sum, x| sum.mul(x))))
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();

            Ok(Matrix(matrix_op(stripped_m, |a, b| a * *b, one)))
        }
    }
}

pub fn div(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() < 1 {
        return Err("Division requires at least one argument!".to_str())
    }

    let literals = try!(unbox_it(args, env));
    let one: BigRational = num::one();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => fail!("Impossible condition!"), //see first test in this fn
        (_    , _    ,  true)   => Err("Attempted boolean subtraction!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed subtraction!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();

            if args.len() == 1 {
                Ok(BigNum(stripped_literals.iter().fold(one, |quot, x| quot.div(x))))
            } else {
                let first = stripped_literals.as_slice()[0].clone();
                let tail = stripped_literals.slice_from(1);
                Ok(BigNum(tail.iter().fold(first, |quot, x| quot.sub(x))))
            }
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();

            Ok(Matrix(matrix_op(stripped_m, |a, b| a / *b, one)))
        }
    }
}
