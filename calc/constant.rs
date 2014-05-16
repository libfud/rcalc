extern crate num;

use self::num::rational::BigRational;
use super::{Evaluate, CalcResult};

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
            _       => Err(("Undefined Constant '" + s  + "'").to_strbuf())
        }
    }
}

impl Evaluate for Constant {
    fn eval(&self) -> CalcResult {
        let &Constant(c_type) = self;
        match c_type {
            Pi => Ok(from_str::<BigRational>("3126535/995207").unwrap()),
            E  => Ok(from_str::<BigRational>("1084483/398959").unwrap())
        }
    }
}
