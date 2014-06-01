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

//doo-wop, doo-wop...
pub fn do_op(args: &Vec<Box<Evaluate>>, env: &mut Environment, min_len: uint,
            op: |BigRational, &BigRational| -> BigRational, ident_fn: || -> BigRational
            ) -> CalcResult {
    if args.len() < min_len {
        return Err(" Specified operation requires at least ".to_str().append(
                    min_len.to_str().as_slice()).append( " arguments!"))
    }

    let literals = try!(unbox_it(args, env));
    let ident: BigRational = ident_fn();

    let (big_flag, bool_flag, matrix_flag) = big_bool_matrix(&literals);
    match (big_flag, matrix_flag, bool_flag) {
        (false, false, false)   => Ok(BigNum(ident_fn())),
        (_    , _    ,  true)   => Err("Attempted nonsense boolean operation!".to_str()),
        (true ,  true, false)   => Err("Attempted mixed operation!".to_str()),
        (true , false, false)   => {
            let stripped_literals: Vec<BigRational> = strip!(literals.move_iter(), BigNum).collect();
            
            if args.len() == 1 {
                Ok(BigNum(stripped_literals.iter().fold(ident, |acc, x| op(acc, x))))
            } else {
                let first = stripped_literals.as_slice()[0].clone();
                let tail = stripped_literals.slice_from(1);
                Ok(BigNum(tail.iter().fold(first, |acc, x| op(acc, x))))
            }
        },
        (false, true , false)   => {
            let stripped_m: Vec<Vec<BigRational>> = strip!(literals.move_iter(), Matrix).collect();
            Ok(Matrix(matrix_op(stripped_m, op, ident)))
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

            Ok(Matrix(matrix_op(stripped_m, |a, b| a / *b, one)))
        }
    }
}
