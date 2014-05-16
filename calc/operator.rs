//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use super::{Evaluate, CalcResult};
use super::common::{rational_to_f64_trig, big_pi, half_circ, str_to_rational};
use super::literal::{LiteralType, Boolean, Matrix, BigNum};

//pub mod power;

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
        literal_vec.push( match (args.get(i).eval()) {
            Ok(good)    => good,
            Err(bad)    => { return Err(bad.to_strbuf()) }
        });
        i += 1;
    }

    Ok(literal_vec)
}

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>) -> CalcResult {
    match op_type {
        Add => {
            let literal_vec = try!(unbox_it(args));

            let zero: BigRational = num::zero();
            let mut matrix_flag = false;
            let mut matrix_len = 0; //Only set once. No matrix ops on inequal matrices.

            //determine if there are any booleans or matrices
            for literal_x in literal_vec.iter() {
                match *literal_x {
                    Boolean(x)  => {
                        return Err("Attempted binary operation with boolean value!".to_strbuf())
                    },
                    Matrix(ref x)   => {
                        if matrix_flag == false { //first matrix encountered
                            matrix_len = x.len();
                            matrix_flag = true;
                        } else {
                            if matrix_len != x.len() {
                                return Err("Inequal matrices".to_strbuf())
                            }
                        }
                    },
                    BigNum(ref x) => { } //do nothing, it might be unnecessary
                }
            }

            //regular adddition
            if matrix_flag == false {
                let mut sum = zero.clone();
                for literal_x in literal_vec.iter() { 
                    sum = sum.add(match *literal_x {
                        Boolean(ref x)  => { &zero }, //taken care of
                        Matrix(ref x)   => { &zero }, //no matrices
                        BigNum(ref x)   => { x }
                    });
                }
                
                Ok(BigNum(sum))

            } else {
                let mut sum_vec: Vec<BigRational> = Vec::new();
                for i in range(0u, matrix_len) { sum_vec.push(zero.clone()); }
                for literal_x in literal_vec.iter() {
                    match *literal_x {
                        Boolean(ref x)  => { }, //taken care of

                        BigNum(ref x)   => {
                            for i in range(0u, matrix_len) { sum_vec.as_slice()[i].add(x); }
                        },

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                sum_vec.as_slice()[i].add(&x.as_slice()[i]);
                            }
                        }
                    }
                }

                Ok(Matrix(sum_vec))
            }
        },

        Sub => {
/*            if args.len() < 1 {
                return Err("Subtraction requires at least one argument".to_strbuf())
            }

            let zero: BigRational = num::zero();
            if args.len() == 1 {
                let answer = zero.sub(&try!(args.get(0).eval()));
                return Ok(answer)
            }

            let first_arg = args.get(0).eval(); */
            Ok(Boolean(true))
        },

        Mul => {
//            let one: BigRational = num::one();
            Ok(Boolean(true))
        },

        Div => {
            if args.len() < 1 {
                return Err("Division requires at least one argument!".to_strbuf())
            }

            let zero: BigRational = num::zero();
/*            if args.len() == 1 && try!(args.get(0).eval()) != zero {
                return Ok(try!(args.get(0).eval()).recip())
            }

            let first_arg = args.get(0).eval();
            if args.slice_from(1).iter().any(|x| x.eval() == Ok(zero.clone())) {
                return Err("Cannot divide by 0".to_strbuf());
            }
            //args.slice_from(1).iter().fold(first_arg, |acc, x| {
            //    combine(acc, x.eval(), |v1, v2| v1.div(&v2))
            //}) */
            Ok(Boolean(true))
        },

        Pow => { //power::pow_wrapper(args) },
            Ok(Boolean(true))
        },

        If  => {
         /*   if args.len() != 3 {
                Err("'if' requires three arguments".to_strbuf())
            } else {
                let condition = try!(args.get(0).eval());
                
                if condition == num::one() {
                    Ok(try!(args.get(1).eval()))
                } else {
                    Ok(try!(args.get(2).eval()))
                }
            }
        */
         Ok(Boolean(true))
        },

        Sin => {
            /*
            if args.len() > 1 {
                return Err("'sin' takes one argument".to_strbuf())
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err("Something went wrong".to_strbuf()) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.sin().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(answer) */
            Ok(Boolean(true))
        },

        Cos => {
            /*
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_strbuf())
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err("Something went wrong".to_strbuf()) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.cos().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(answer) */
            Ok(Boolean(true))
        },

        Tan => {
            /*
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_strbuf())
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err("Something went wrong".to_strbuf()) }
            };

            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = (ration_as_float.sin() / ration_as_float.cos()).to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_strbuf()) }
            };
            
            Ok(answer)
            */
            Ok(Boolean(true))
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
            Ok(Boolean(true))
        },

        LtEq => {
            if args.len() != 2 {
                return Err("<= requires two arguments".to_strbuf())
            }
            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(Boolean(true))
        },

        Eq  => {
            if args.len() != 2 {
                return Err("= requires two arguments".to_strbuf())
            }

            let arg1 = try!(args.get(0).eval());
            let arg2 = try!(args.get(1).eval());

            Ok(Boolean(true))
        },

        GtEq => {
            if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(Boolean(true))
        },
        
        Gt   => {
             if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(Boolean(true))
        }
    }
}

