//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;

#[deriving(Show, Clone, Ord, Eq)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Func(String)
}

