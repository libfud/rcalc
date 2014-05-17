//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;

#[deriving(Show)]
#[deriving(Clone)]
#[deriving(Ord)]
#[deriving(Eq)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Matrix(Vec<BigRational>)
}
