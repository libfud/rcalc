//!Constants

extern crate num;

use self::num::rational::BigRational;
use super::{Evaluate, CalcResult, Environment};
use super::literal::{BigNum, LiteralType};

#[deriving(Show)]
pub enum ConstantType {
    Pi,
    E
}

pub struct Constant(pub ConstantType);

impl Constant {
    pub fn from_str(s: &str) -> CalcResult<Constant> {
        match s {
            "pi"    => Ok(Constant(Pi)),
            "e"     => Ok(Constant(E)),
            _       => Err(("Undefined Constant :".to_str().append(s.to_str().as_slice())))
        }
    }
}

impl Evaluate for Constant {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        let &Constant(c_type) = self;
        match c_type {
            Pi => Ok(BigNum(from_str::<BigRational>("3126535/995207").unwrap())),
            E  => Ok(BigNum(from_str::<BigRational>("1084483/398959").unwrap()))
        }
    }
}

pub fn from_const_str(s: &str) -> Option<LiteralType> {
    match s {
        "pi"    => Some(BigNum(from_str::<BigRational>("3126535/995207").unwrap())),
        "e"     => Some(BigNum(from_str::<BigRational>("1084483/398959").unwrap())),
        _       => None
    }
}
