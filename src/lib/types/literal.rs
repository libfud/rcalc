//! An enumeration of valid literaltypes

extern crate num;
extern crate matrix;

use self::matrix::{Matrice};
use super::{BigRational, CalcResult, Expression, Environment};
use std::num;
use std::num::{Zero, One};
use std::fmt;

#[deriving(Clone, PartialEq, PartialOrd)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    List(Vec<LiteralType>),
    Matrix(Matrice<LiteralType>),
    Proc(Vec<String>, Expression),
    Symbol(String),
    Void
}

pub struct WithEnv<'a> {
    env: &'a Environment,
    data: &'a LiteralType
}

impl<'a> fmt::Show for WithEnv<'a> {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            &Boolean(ref x) => print!("{}", x),
            &BigNum(ref x) => if x.is_integer() {
                print!("{}", x.numer())
            } else {
                print!("{}", x)
            },
            &List(ref list) => print!("{}", list),
            &Matrix(ref m) => print!("{}", m),
            &Proc(ref args, ref expr) => {
                print!("Procedure: parameters: {}, body: {}", args, expr)
            },
            &Symbol(ref s) => print!("{} {}", s, match self.env.lookup(s) {
                Ok(x) => format!("= {}", x),
                Err(m) => m.to_str(),
            }),
            &Void => print!("")
        }

        Ok(())
    }
}

impl fmt::Show for LiteralType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Boolean(ref x) => try!(write!(fmt, "{}", x)),
            &BigNum(ref x) => if x.is_integer() {
                try!(write!(fmt, "{}", x.numer()))
            } else {
                try!(write!(fmt, "{}", x))
            },
            &List(ref list) => try!(write!(fmt, "{}", list)),
            &Matrix(ref m) => try!(write!(fmt, "{}", m)),
            &Proc(ref args, ref expr) => {
                try!(write!(fmt, "Procedure: parameters: {}, body: {}", args, expr))
            },
            &Symbol(ref s) => try!(write!(fmt, "{}", s)),
            &Void => try!(write!(fmt, ""))
        }

        Ok(())
    }
}

pub type Lit = LiteralType;
pub type LitRes =  CalcResult<LiteralType>;

impl Num for Lit { }

impl Zero for Lit {
    fn zero() -> Lit {
        BigNum(num::zero())
    }

    fn is_zero(&self) -> bool {
        match self {
            &BigNum(ref x) => x.is_zero(),
            _ => false
        }
    }
}

impl One for Lit {
    fn one() -> Lit {
        BigNum(num::one())
    }
}

impl Neg<Lit> for Lit {
    fn neg(&self) -> Lit {
        match self {
            &BigNum(ref x) => BigNum(-x),
            &Matrix(ref x) => Matrix(-x),
            _ => fail!("Can't negate something that isn't a number!".to_str())
        }
    }
}

impl Add<Lit, Lit> for Lit {
    fn add(&self, rhs: &LiteralType) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x + *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x + *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a + *b)),
            (&BigNum(_), &Matrix(ref x)) => Matrix(x.scalar(self, |a, b| a + *b)),
            _ => fail!(format!("Arithmetic not defined for {} {}", self, rhs))
        }
    }
}

impl Sub<Lit, Lit> for Lit {
    fn sub(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x - *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x - *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a - *b)),
            _ => fail!(format!("Arithmetic not defined for {} {}", self, rhs))
        }
    }
}

impl Mul<Lit, Lit> for Lit {
    fn mul(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x * *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x * *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a * *b)),
            (&BigNum(_), &Matrix(ref x)) => Matrix(x.scalar(self, |a, b| a * *b)),
            _ => fail!(format!("Arithmetic not defined for {} {}", self, rhs))
        }

    }
}

impl Div<Lit, Lit> for Lit {
    fn div(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x / *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x / *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a / *b)),
             _ => fail!("Division is only defined for numbers".to_str())
        }
    }
}

impl Rem<Lit, Lit> for Lit {
    fn rem(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x % *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x % *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a % *b)),
            _ => fail!("Rem is only defined for numbers".to_str())
        }
    }
}

