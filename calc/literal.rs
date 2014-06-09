//! An enumeration of valid literaltypes

extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Environment};
use super::lookup;

#[deriving(Show, Clone, PartialEq, PartialOrd)]
pub enum BasicType<'a, T> {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Lambda(Box<Vec<BasiclType<'a, T>>>, |Vec<Foo<'a, T>>|:'a -> T),
}

pub enum Expression {
    Atom(BasicType),
    Cons(Box<Expression>, Box<Expression>)
}

/*
pub enum AbstractType {
    Atom(LiteralType),
    Set(Vec<LiteralType>), //all same type
    List(Vec<Box<AbstractType>>), //varying types
    Array(Vec<Box<AbstractType>>), //all same type
    Matrix(Vec<Vec<Box<AbstractType>>>) //all same type
}
*/
