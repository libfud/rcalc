//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use std::str::Slice;
use super::{Evaluate, CalcResult};
use super::expression::combine;
use super::common::{rational_to_f64_trig, big_pi, half_circ, str_to_rational};

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

fn bool_to_bigrational(b: bool) -> BigRational {
    if b { num::one() } else { num::zero() }
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
                return Err(Slice("Subtraction requires at least one argument"))
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
                return Err(Slice("Division requires at least one argument!"))
            }

            let zero: BigRational = num::zero();
            if args.len() == 1 && try!(args.get(0).eval()) != zero {
                return Ok(try!(args.get(0).eval()).recip())
            }

            let first_arg = args.get(0).eval();
            if args.slice_from(1).iter().any(|x| x.eval() == Ok(zero.clone())) {
                return Err(Slice("Cannot divide by 0"));
            }
            args.slice_from(1).iter().fold(first_arg, |acc, x| {
                combine(acc, x.eval(), |v1, v2| v1.div(&v2))
            })

        },

        Pow => { power::pow_wrapper(args) },

        If  => {
            if args.len() != 3 {
                Err(Slice("[if' requires three arguments"))
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
                return Err(Slice("'sin' takes one argument"))
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err(Slice("Something went wrong")) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.sin().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(Slice(msg)) }
            };
            
            Ok(answer)
        },

        Cos => {
            if args.len() > 1 {
                return Err(Slice("'cos' takes one argument"))
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err(Slice("Something went wrong")) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.cos().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(Slice(msg)) }
            };
            
            Ok(answer)
        },

        Tan => {
            if args.len() > 1 {
                return Err(Slice("'cos' takes one argument"))
            }
            let evaluated = match args.get(0).eval() {
                Ok(value)    => value,
                Err(_)    => { return Err(Slice("Something went wrong")) }
            };

            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = (ration_as_float.sin() / ration_as_float.cos()).to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(Slice(msg)) }
            };
            
            Ok(answer)
        },

        Rad => {
            if args.len() != 1 {
                return Err(Slice("'rad' takes one argument"))
            }
            
            let degrees = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let radians = degrees.mul(&pi.div(&one80));

            Ok(radians)
        },

        Deg => {
            if args.len() != 1 {
                return Err(Slice("'rad' takes one argument"))
            }

            let radians = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let degrees = radians.mul(&one80.div(&pi));

            Ok(degrees)
        },

        Lt  => {
            if args.len() != 2 {
                return Err(Slice("< requires two arguments"))
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(bool_to_bigrational(arg1 < arg2))
        },

        LtEq => {
            if args.len() != 2 {
                return Err(Slice("<= requires two arguments"))
            }
            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(bool_to_bigrational(arg1 <= arg2))
        },

        Eq  => {
            if args.len() != 2 {
                return Err(Slice("= requires two arguments"))
            }

            let arg1 = try!(args.get(0).eval());
            let arg2 = try!(args.get(1).eval());

            Ok(bool_to_bigrational(arg1 == arg2))
        },

        GtEq => {
            if args.len() != 2 {
                return Err(Slice(">= requires two arguments"))
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(bool_to_bigrational(arg1 >= arg2))
        },
        
        Gt   => {
             if args.len() != 2 {
                return Err(Slice(">= requires two arguments"))
            }

            let (arg1, arg2) = (try!(args.get(0).eval()), try!(args.get(1).eval()));
            Ok(bool_to_bigrational(arg1 > arg2))
        }
    }
}

