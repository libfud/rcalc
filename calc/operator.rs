//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use super::{Evaluate, CalcResult};
use super::common::{rational_to_f64_trig, str_to_rational};
use super::literal::{LiteralType, Boolean, Matrix, BigNum};

pub mod power;

#[deriving(Show)]
#[deriving(Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Sin,
    Cos,
    Tan,
    Rad,
    Deg,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    If,
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+"     => Some(Add),
        "-"     => Some(Sub),
        "*"     => Some(Mul),
        "/"     => Some(Div),
        "pow"   => Some(Pow),
        "sin"   => Some(Sin),
        "cos"   => Some(Cos),
        "tan"   => Some(Tan),
        "rad"   => Some(Rad),
        "deg"   => Some(Deg),
        "<"     => Some(Lt),
        "<="    => Some(LtEq),
        "="     => Some(Eq),
        ">="    => Some(GtEq),
        ">"     => Some(Gt),
        "if"    => Some(If),
        _       => None
    }
}

pub fn unbox_it(args:&Vec<Box<Evaluate>>) -> Result<Vec<LiteralType>, StrBuf> {
    let mut literal_vec: Vec<LiteralType> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        literal_vec.push( match args.get(i).eval() {
            Ok(good)    => good,
            Err(bad)    => { return Err(bad.to_strbuf()) }
        });
        i += 1;
    }

    Ok(literal_vec)
}

pub fn big_bool_matrix(args: &Vec<LiteralType>) -> (bool, bool, bool) {
    let mut bignum_flag = false;
    let mut bool_flag = false; //lol
    let mut matrix_flag = false;

    for literal in args.iter() {
        match literal {
            &BigNum(_)  => bignum_flag = true,
            &Boolean(_) => bool_flag = true,
            &Matrix(_)  => matrix_flag = true,
        }
    }

    (bignum_flag, bool_flag, matrix_flag)
}

pub fn find_matrix_len(args: &Vec<LiteralType>) -> Result<uint, StrBuf> {
    let mut matrix_len = 0;

    //get length of first matrix
    for literal in args.iter() {
        match literal {
            &Matrix(ref x)   => {
                matrix_len = x.len();
                break;
            }
            _           => { } //do nothing
        }
    }

    //see if all matrices are of a uniform length
    for literal in args.iter() {
        match literal {
            &Matrix(ref x)   => {
                if x.len() != matrix_len {
                    return Err("Mismatched matrices.".to_strbuf())
                }
            }
            _           => { } //do nothing
        }
    }
    
    if matrix_len == 0 {
        Err("0 length matrices are not allowed!".to_strbuf())
    } else {
        Ok(matrix_len)
    }
}

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>) -> CalcResult {
    match op_type {
        Add => {
            let literal_vec = try!(unbox_it(args));

            fn add_big(terms: &Vec<BigRational>) -> BigRational {
                let mut sum: BigRational = num::zero();
                for x in terms.iter() { sum = sum.add(x) }
                sum
            };

            let matrix_add = | terms: &Vec<Vec<BigRational>> | -> Vec<BigRational> {
                let mut sum_vec: Vec<BigRational> = Vec::new();
                let matrix_len= match find_matrix_len(&literal_vec) {
                    Ok(val) => val,
                    Err(_) => 0, //this shouldn't happen
                };
                let len = terms.len();

                for i in range(0u, matrix_len) {
                    let mut column: Vec<BigRational> = Vec::new();
                    for x in range(0u, len) {
                        column.push(terms.as_slice()[x].as_slice()[i].clone());
                    }
                    sum_vec.push(add_big(&column.clone()));
                }

                sum_vec
            };

            let (bignum_flag, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            match (bignum_flag, matrix_flag, bool_flag) {
                (false, false, false)   => { Ok(BigNum(num::zero())) }
                (_, _, true)    => {
                    Err("Attempted addition with boolean value!".to_strbuf())
                },

                (true, true, false) => {
                    Err("Cannot currently add matrices and bignums".to_strbuf())
                },
                (true, false, false)    => {
                    let mut vec_bigs: Vec<BigRational> = Vec::new();
                    for term in literal_vec.iter() {
                        vec_bigs.push(match *term{
                            BigNum(ref x)   => x.clone(),
                            _           => return Err("impssible".to_strbuf())
                        });
                    }
                    Ok(BigNum(add_big(&vec_bigs.clone())))
                },
                (false, true, false)    => {
                    let mut vec_matrix: Vec<Vec<BigRational>> = Vec::new();
                    for term in literal_vec.iter() {
                        vec_matrix.push(match *term {
                            Matrix(ref x)   => x.clone(),
                            _           => return Err("impossible".to_strbuf())
                        });
                    }
                    Ok(Matrix(matrix_add(&vec_matrix.clone())))
                }
            }
        },

        Sub => {
            if args.len() < 1 {
                return Err("Subtraction requires at least one argument".to_strbuf())
            }

            let literal_vec = try!(unbox_it(args));

            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted subtraction with boolean value!".to_strbuf())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            let zero: BigRational = num::zero();

            fn bignum_sub(head: BigRational, tail: &[LiteralType]) -> BigRational {
                let mut difference = head.clone();

                for term in tail.iter() {
                    match *term {
                        BigNum(ref x) => difference = difference.sub(x),
                        _             => { },
                    }
                }

                difference
            }

            fn matrix_sub(matrix_len: uint, head: &[BigRational], tail: &[LiteralType])
                -> Vec<BigRational> {

                let mut diff_vec: Vec<BigRational> = Vec::new();
                for i in range(0u, matrix_len) { diff_vec.push(head[i].clone()); }

                for literal in tail.iter(){
                    match *literal {
                        BigNum(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let diff = diff_vec.as_slice()[i].sub(x);
                                diff_vec.as_mut_slice()[i] = diff;
                            }
                        },
                        Boolean(_)  => { }, //no booleans
                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let diff = diff_vec.as_slice()[i].sub(&x.as_slice()[i]);
                                diff_vec.as_mut_slice()[i] = diff;
                            }
                        },
                    }
                }

                diff_vec
            }

            if matrix_flag == false {
                if args.len() == 1 {
                    Ok(BigNum(bignum_sub(zero, literal_vec.as_slice())))
                }
                else {
                    let head = match literal_vec.as_slice()[0] {
                        BigNum(ref x)   => x.clone(),
                        _               => zero.clone()
                    };

                    Ok(BigNum(bignum_sub(head, literal_vec.slice_from(1))))
                }
            } else { 
                let mut head: Vec<BigRational> = Vec::new();
                let mut head_i = 0;

                if args.len() == 1 { 
                    for _ in range(0u, matrix_len) { head.push(zero.clone()); }
                } else {
                    head_i = 1;
                    match literal_vec.as_slice()[0] {
                        BigNum(_)       => {
                            return Err(("Illegal subtraction operation! "
                            + "Cannot subtract matrix from bignum") .to_strbuf())
                        },

                        Boolean(_)      => { }, //there are no booleans

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                head.push(x.as_slice()[i].clone());
                            }
                        }
                    }
                }

                Ok(Matrix(matrix_sub(matrix_len, head.as_slice(), literal_vec.slice_from(head_i))))
            }
        },

        Mul => {
            let literal_vec = try!(unbox_it(args));

            let (_,bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted multiplication with boolean value!".to_strbuf())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            let one: BigRational = num::one();

            if matrix_flag == false {
                let mut product = one.clone();
                for literal in literal_vec.iter() {
                    match *literal {
                        BigNum(ref x)   => product = product.mul(x),
                        _               => { }//do nothing
                    }
                }

                Ok(BigNum(product))
            } else {
                let mut prod_vec: Vec<BigRational> = Vec::new();
                for _ in range(0u, matrix_len) { prod_vec.push(one.clone()) }

                for literal_x in literal_vec.iter(){
                    match *literal_x {
                        Boolean(_)      => { }, //do nothing

                        BigNum(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let product = prod_vec.as_slice()[i].mul(x);
                                prod_vec.as_mut_slice()[i] = product;
                            }
                        },

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let product = prod_vec.as_slice()[i].mul(&x.as_slice()[i]);
                                prod_vec.as_mut_slice()[i] = product;
                            }
                        }
                    }
                }

                Ok(Matrix(prod_vec))
            }
        },

        Div => {
            if args.len() < 1 {
                return Err("Division requires at least one argument!".to_strbuf())
            }

            let literal_vec = try!(unbox_it(args));

            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted multiplication with Boolean value!".to_strbuf())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            fn bignum_div(head: BigRational, tail: &[LiteralType]) -> 
                Result<BigRational, StrBuf> {

                let mut quotient = head.clone();
                let zero: BigRational = num::zero();

                for term in tail.iter() {
                    match *term {
                        BigNum(ref x)   => {
                            if *x == zero { return Err("Division by zero!".to_strbuf()) }
                            quotient = quotient.div(x);
                            }
                        _               => { },
                    }
                }

                Ok(quotient)
            }   

            fn matrix_div(matrix_len: uint, head: &[BigRational], tail: &[LiteralType]) ->
                Result<Vec<BigRational>, StrBuf> {
                
                let zero: BigRational = num::zero();
                let mut quot_vec: Vec<BigRational> = Vec::new();
                for i in range(0u, matrix_len) { quot_vec.push(head[i].clone()); }

                for literal in tail.iter() {
                    match *literal {
                        BigNum(ref x)   => {
                            if *x == zero {
                                return Err("Division by zero!".to_strbuf())
                            }
                            for i in range(0u, matrix_len) {

                                let quotient = quot_vec.as_slice()[i].div(x);
                                quot_vec.as_mut_slice()[i] = quotient;
                            }
                        }
                        Boolean(_)      => { },
                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                if x.as_slice()[i] == zero {
                                    return Err("Division by zero!".to_strbuf())
                                }
                                let quotient = quot_vec.as_slice()[i].div(&x.as_slice()[i]);
                                quot_vec.as_mut_slice()[i] = quotient;
                            }
                        }
                    }
                }

                Ok(quot_vec)
            }

            let one: BigRational = num::one();
            if matrix_flag == false {
                if args.len() == 1 {
                    let answer = try!(bignum_div(one, literal_vec.as_slice()));

                    Ok(BigNum(answer))
                } else {
                    let head = match literal_vec.as_slice()[0] {
                        BigNum(ref x)   => x.clone(),
                        _               => one.clone(), //shouldn't happen, see matrix_flag
                    };

                    let answer = try!(bignum_div(head, literal_vec.slice_from(1)));

                    Ok(BigNum(answer))
                }
            } else {
                let mut head: Vec<BigRational> = Vec::new();
                let mut head_i = 0;

                if args.len() == 1 {
                    for _ in range(0u, matrix_len) { head.push(one.clone()); }
                } else {
                    head_i = 1;

                    match literal_vec.as_slice()[0] {
                        BigNum(_)       => {
                            return Err(("Illegal division operation! " + 
                            "Cannot divide bignum by matrix!").to_strbuf())
                        },

                        Boolean(_)      => {}, //booleans already caused failure if present

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                head.push(x.as_slice()[i].clone());
                            }
                        }
                    }
                }

                let answer = try!(matrix_div(matrix_len, head.as_slice(),
                    literal_vec.slice_from(head_i)));

                Ok(Matrix(answer))
            }
        },

        Pow => {
            let literal_vec = try!(unbox_it(args));
            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true || matrix_flag == true {
                return Err("Not yet...".to_strbuf())
            }
            power::pow_wrapper(args) },

        If  => {
            if args.len() != 3 {
                return Err("'if' requires three arguments".to_strbuf())
            } 
            
            let condition = match try!(args.get(0).eval()) {
                Boolean(x)  => x,
                _           => { return Err("Only booleans can be a condition!".to_strbuf()) }
            };
                
            if condition == true {
                Ok(try!(args.get(1).eval()))
            } else {
                Ok(try!(args.get(2).eval()))
            }
        },

        Sin => {
            if args.len() > 1 {
                return Err("'sin' takes one argument".to_strbuf())
            }

            let evaluated_array = try!(unbox_it(args));
            let evaluated = match evaluated_array.as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => {
                    return Err("I'm too tired to do this right now.".to_strbuf())
                },
            };
            
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.sin().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(BigNum(answer))
        },

        Cos => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_strbuf())
            }
            let evaluated = match (try!(unbox_it(args))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Something went wrong".to_strbuf()) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.cos().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(BigNum(answer))
        },

        Tan => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_strbuf())
            }
            let evaluated = match (try!(unbox_it(args))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Too tired".to_strbuf()) }
            };

            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = (ration_as_float.sin() / ration_as_float.cos()).to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(BigNum(answer))
        },

        Rad => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_strbuf())
            }
            
            let degrees = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let radians = degrees.mul(&pi.div(&one80));

            Ok(radians) */
            Ok(Boolean(true))
        },

        Deg => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_strbuf())
            }

            let radians = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let degrees = radians.mul(&one80.div(&pi));

            Ok(degrees)
            */
            Ok(Boolean(true))
        },

        Lt  => {
            if args.len() != 2 {
                return Err("< requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 < arg2)),
                _   => Err("Nonboolean".to_strbuf())
            }
        },

        LtEq => {
            if args.len() != 2 {
                return Err("<= requires two arguments".to_strbuf())
            }
            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 <= arg2)),
                _   => Err("Non boolean".to_strbuf())
            }
        },

        Eq  => {
            if args.len() != 2 {
                return Err("= requires two arguments".to_strbuf())
            }

            let arg1 = try!(args.get(0).eval());
            let arg2 = try!(args.get(1).eval());
            match (arg1, arg2) {
                (BigNum(x), BigNum(y))  => Ok(Boolean(x == y)),
                _                       => Err("oh snap".to_strbuf())
            }
        },

        GtEq => {
            if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 >= arg2)),
                _                       => Err("something".to_strbuf())
            }
        },
        
        Gt   => {
             if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 > arg2)),
                _                       => Err("blug".to_strbuf())
            }
        }
    }
}

