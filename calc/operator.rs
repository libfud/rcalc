//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use std::str::Slice;
use super::{Evaluate, CalcResult};
use super::expression::combine;

#[deriving(Show)]
#[deriving(Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
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
        "<"     => Some(Lt),
        "<="    => Some(LtEq),
        "="     => Some(Eq),
        ">="    => Some(GtEq),
        ">"     => Some(Gt),
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

            let mut equal = false;
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
