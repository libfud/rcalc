//!Basic arithemtic functions. 

extern crate num;

use std::num;
use super::{big_bool_matrix, unbox_it};
use super::super::literal::{Matrix, BigNum};
use super::super::{CalcResult, Environment, Evaluate};
use self::num::rational::BigRational;

pub fn add(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    let literals = try!(unbox_it(args, env));
    let zero: BigRational = num::zero();

    let matrix_add = |terms: &Vec<Vec<BigRational>>| -> Vec<BigRational> {
        let mut sum_vec: Vec<BigRational> = Vec::new();
        let matrix_len = terms.as_slice()[0].len();
        let zero2: BigRational = num::zero();

        for i in range(0u, matrix_len) {
            let column: Vec<BigRational> = terms.iter().fold(vec![], |mut acc, elem| {
                acc.push(elem.as_slice()[i].clone());
                acc
            });

            sum_vec.push(column.iter().fold(zero2.clone(), |acc, x| acc.add(x)));
        }

        sum_vec
    };

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => Ok(BigNum(num::zero())),
        (_    , _    ,  true)   => Err("Attempted boolean addition!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed addition!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = literals.iter().map(|x|
                match x {
                    &BigNum(ref n)  => n.clone(),
                    _   => fail!("Impossible!")
                }
            ).collect();
            Ok(BigNum(stripped_literals.iter().fold(zero, |acc, x| acc.add(x))))
        },
        (false, true , false)   => {
            let stripped_matrix: Vec<Vec<BigRational>> = literals.iter().map(|x|
                match x {
                    &Matrix(ref v)  => v.clone(),
                    _   => fail!("Impossible!")
                }
            ).collect();
            Ok(Matrix(matrix_add(&stripped_matrix.clone())))
        }
    }
}

pub fn sub(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() < 1 {
        return Err("Subtraction requires at least one argument".to_str())
    }

    let literals = try!(unbox_it(args, env));

    let zero: BigRational = num::zero();

    let matrix_sub = |terms: &Vec<Vec<BigRational>>| -> Vec<BigRational> {
        let mut diff_vec: Vec<BigRational> = Vec::new();
        let matrix_len = terms.as_slice()[0].len();
        let zero2: BigRational = num::zero();

        for i in range(0u, matrix_len) {
            let column: Vec<BigRational> = terms.iter().fold(vec![], |mut acc, elem| {
                acc.push(elem.as_slice()[i].clone());
                acc
            });
            
            match terms.len() {
                1   => {
                    diff_vec.push(column.iter().fold(zero2.clone(), |acc, x| acc.sub(x)));
                },
                _   => {
                    let head = column.as_slice()[0].clone();
                    let tail = column.slice_from(1);
                    diff_vec.push(tail.iter().fold(head, |acc, x| acc.sub(x)));
                }
            }
        }

        diff_vec
    };

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => fail!("Impossible condition!"), //see first test in this fn
        (_    , _    ,  true)   => Err("Attempted boolean subtraction!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed subtraction!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = literals.iter().map(|x|
                match x {
                    &BigNum(ref n)  => n.clone(),
                    _   => fail!("Impossible!")
                }
            ).collect();
            if args.len() == 1 {
                Ok(BigNum(stripped_literals.iter().fold(zero, |acc, x| acc.sub(x))))
            } else {
                let first = stripped_literals.as_slice()[0].clone();
                let tail = stripped_literals.slice_from(1);
                Ok(BigNum(tail.iter().fold(first, |acc, x| acc.sub(x))))
            }
        },
        (false, true , false)   => {
            let stripped_matrix: Vec<Vec<BigRational>> = literals.iter().map(|x|
                match x {
                    &Matrix(ref v)  => v.clone(),
                    _   => fail!("Impossible!")
                }
            ).collect();

            Ok(Matrix(matrix_sub(&stripped_matrix)))
        }
    }
}
