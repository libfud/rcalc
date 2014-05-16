//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use super::{Evaluate, CalcResult};
use super::expression::combine;
use super::common::{rational_to_f64_trig, big_pi, half_circ, str_to_rational};
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

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>) -> CalcResult {
    match op_type {
        Add => {
            let zero: BigRational = num::zero();
            args.iter().fold(Ok(zero), |acc, x| {
                combine(acc, x.eval(), |v1, v2| v1.add(&v2))
            })
        },

        Sub => {
            if args.len() < 1 {
                return Err("Subtraction requires at least one argument".to_strbuf())
            }

            let zero: BigRational = num::zero();
            if args.len() == 1 {
                let answer = zero.sub(&try!(args.get(0).eval()));
                return Ok(answer)
            }

            let first_arg = args.get(0).eval();
            args.slice_from(1).iter().fold(first_arg, |acc, x| {
                combine(acc, x.eval(), |v1, v2| v1.sub(&v2))
            })
        },

        Mul => {
            let one: BigRational = num::one();
            args.iter().fold(Ok(one), |acc, x| {
                combine(acc, x.eval(), |v1, v2| v1.mul(&v2))
            })
        },

        Div => {
            if args.len() < 1 {
                return Err("Division requires at least one argument!".to_strbuf())
            }

            let zero: BigRational = num::zero();
            if args.len() == 1 && try!(args.get(0).eval()) != zero {
                return Ok(try!(args.get(0).eval()).recip())
            }

            let first_arg = args.get(0).eval();
            if args.slice_from(1).iter().any(|x| x.eval() == Ok(zero.clone())) {
                return Err("Cannot divide by 0".to_strbuf());
            }
            args.slice_from(1).iter().fold(first_arg, |acc, x| {
                combine(acc, x.eval(), |v1, v2| v1.div(&v2))
            })

        },

        Pow => { power::pow_wrapper(args) },

        If  => {
            if args.len() != 3 {
                Err("'if' requires three arguments".to_strbuf())
            } else {
                let condition = try!(args.get(0).eval());
                
                if condition == num::one() {
                    Ok(try!(args.get(1).eval()))
                } else {
                    Ok(try!(args.get(2).eval()))
                }
            }
        },

        Sin => {
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
            
            Ok(answer)
        },

        Cos => {
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
            
            Ok(answer)
        },

        Tan => {
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
        },

        Rad => {
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_strbuf())
            }
            
            let degrees = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let radians = degrees.mul(&pi.div(&one80));

            Ok(radians)
        },

        Deg => {
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_strbuf())
            }

            let radians = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let degrees = radians.mul(&one80.div(&pi));

            Ok(degrees)
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
            Ok(Boolean(arg1 <= arg2))
        },

        Eq  => {
            if args.len() != 2 {
                return Err("= requires two arguments".to_strbuf())
            }

            let arg1 = try!(args.get(0).eval());
            let arg2 = try!(args.get(1).eval());

            Ok(Boolean(arg1 == arg2))
        },

        GtEq => {
            if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(Boolean(arg1 >= arg2))
        },
        
        Gt   => {
             if args.len() != 2 {
                return Err(">= requires two arguments".to_strbuf())
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(Boolean(arg1 > arg2))
        }
    }
}

