
use std::num;
use std::str::{Slice, Owned};
use super::{Evaluate, CalcResult};

pub mod power;

#[deriving(Show)]
pub enum FunctionType {
//    Sin,
//    Cos,
//    Tan,
    Pow,
    If
}

pub fn eval(f_type: FunctionType, args: &Vec<Box<Evaluate>>) -> CalcResult {
    match f_type {
        Pow => { power::pow_wrapper(args) }
        If  => {
            if args.len() != 3 {
                Err(Slice("'if' requires three arguments"))
            } else {
                let condition = try!(args.get(0).eval());

                if condition == num::one() {
                    Ok(try!(args.get(1).eval()))
                } else {
                    Ok(try!(args.get(2).eval()))
                }
            }
        }
    }
}

pub fn from_str(name: &str) -> CalcResult<FunctionType> {
    match name {
        "pow"   => Ok(Pow),
        "if"    => Ok(If),
        _       => Err(Owned(format!("Unknown function '{}'", name)))
    }
}
