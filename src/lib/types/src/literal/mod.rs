//! An enumeration of valid literaltypes

extern crate num;
extern crate matrix;

use self::num::{BigRational, BigInt, One, Zero, Num};
use self::num::traits::CheckedAdd;
use self::matrix::{Matrice, SquareRoot};
use super::record::{ProtoRecord, Record};
use super::ErrorKind::{UnexpectedVal};
use super::{Expression, CalcResult, Environment, ErrorKind};
use std::fmt;
use std::ops::{Add,Sub,Neg,Mul,Div,Rem};
use std::num::ToPrimitive;

pub type Lit = LiteralType;
pub type LitRes =  CalcResult<LiteralType>;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum LiteralType {
    Boolean(bool),
    BigNum(BigRational),
    List(Vec<LiteralType>),
    Matrix(Matrice<LiteralType>),
    Proc(Vec<String>, Expression),
    Proto(ProtoRecord),
    Structure(Record),
    Symbol(String),
    Void
}

#[derive(Debug, PartialOrd, PartialEq, Eq, Ord, Clone)]
pub struct Procedure {
    params: Vec<String>,
    body: Expression
}

impl fmt::Display for Procedure {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "parameters: ("));
        for item in self.params.iter() {
            try!(write!(fmt, "{} ,", item));
        }
        
        try!(write!(fmt, ") , body: {}", self.body));
        Ok(())
    }

}

impl<'a> Procedure {
    #[inline]
    pub fn new(params: &Vec<String>, body: &Expression) -> Procedure {
        Procedure { params: params.clone(), body: body.clone() }
    }

    #[inline]
    pub fn to_lit(&self) -> Lit {
        LiteralType::Proc(self.params.clone(), self.body.clone())
    }

    #[inline]
    pub fn destruct(&'a self) -> (&'a Vec<String>, &'a Expression) {
        (&self.params, &self.body)
    }
}

pub struct WithEnv<'a> {
    pub env: &'a Environment,
    pub data: &'a LiteralType
}

impl<'a> fmt::Display for WithEnv<'a> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            &LiteralType::Boolean(ref x) => try!(write!(fmt, "{}", x)),
            &LiteralType::BigNum(ref x) => try!(write!(fmt, "{}", x)),
            &LiteralType::List(ref list) => {
                try!(write!(fmt, "("));
                for elt in list.init().iter() {
                    try!(write!(fmt, "{} ", elt));
                }
                try!(write!(fmt, "{})", match list.last() {
                    Some(x) => x.to_string(),
                    None => "".to_string()
                }));
            },
            &LiteralType::Matrix(ref m) => try!(write!(fmt, "{}", m)),
            &LiteralType::Proc(ref a, ref e) => {
                try!(write!(fmt, "Procedure: parameters: ("));
                for item in a.iter() {
                    try!(write!(fmt, "{} ,", item));
                }
                
                try!(write!(fmt, ") , body: {}", e));
            },

            &LiteralType::Proto(ref p) => try!(write!(fmt, "{}", p)),
            &LiteralType::Structure(ref rec) => try!(write!(fmt, "{}", rec)),
            &LiteralType::Symbol(ref s) => try!(write!(fmt, "{} {}", s, match self.env.lookup(s) {
                Ok(x) => format!("= {}", x),
                Err(m) => m.to_string(),
            })),
            &LiteralType::Void => ()
        }

        Ok(())
    }
}

impl fmt::Display for LiteralType {
    #[inline]   
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LiteralType::Boolean(ref x) => try!(write!(fmt, "{}", x)),
            LiteralType::BigNum(ref x) => try!(write!(fmt, "{}", x)),
            LiteralType::List(ref list) => {
                try!(write!(fmt, "("));
                for elt in list.init().iter() {
                    try!(write!(fmt, "{} ", elt));
                }
                try!(write!(fmt, "{})", match list.last() {
                    Some(x) => x.to_string(),
                    None => "".to_string()
                }));
            }
            LiteralType::Matrix(ref m) => try!(write!(fmt, "{}", m)),
            LiteralType::Structure(ref rec) => try!(write!(fmt, "{}", rec)),
            LiteralType::Proc(ref a, ref e) => {
                try!(write!(fmt, "Procedure: parameters: ("));
                for item in a.iter() {
                    try!(write!(fmt, "{} ,", item));
                }
                
                try!(write!(fmt, ") , body: {}", e));
            },
            LiteralType::Proto(ref p) => try!(write!(fmt, "{}", p)),
            LiteralType::Symbol(ref s) => try!(write!(fmt, "{}", s)),
            LiteralType::Void => ()
        }

        Ok(())
    }
}

impl<'a> LiteralType {
    #[inline]
    pub fn to_bool(&self) -> CalcResult<bool> {
        match self {
            &LiteralType::Boolean(ref b) => Ok(*b),
            x => Err(UnexpectedVal("LiteralType::Boolean".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        match self {
            &LiteralType::Boolean(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_bignum(&self) -> CalcResult<BigRational> {
        match self {
            &LiteralType::BigNum(ref bignum) => Ok(bignum.clone()),
            x => Err(ErrorKind::UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_num(&self) -> bool {
        match self {
            &LiteralType::BigNum(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_int(&self) -> CalcResult<isize> {
        match self {
            &LiteralType::BigNum(ref bignum) => match bignum.to_integer().to_isize() {
                Some(x) => Ok(x),
                None => Err(super::ErrorKind::BadArgType("Couldn't convert to int".to_string()))
            },
            x => Err(ErrorKind::UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn to_usize(&self) -> CalcResult<usize> {
        match self {
            &LiteralType::BigNum(ref bignum) => match bignum.to_integer().to_usize() {
                Some(x) => Ok(x),
                None => Err(super::ErrorKind::BadArgType("Couldn't convert to uint".to_string()))
            },
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn to_f64(&self) -> CalcResult<f64> {
        match self {
            &LiteralType::BigNum(ref big) => match (big.numer().to_f64(), big.denom().to_f64()) {
                (Some(x), Some(y)) => Ok((x / y)),
                (_, _) => return Err(super::ErrorKind::BadFloatRange)
            },
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }        

    #[inline]
    pub fn to_vec(&self) -> CalcResult<Vec<LiteralType>> {
        match self {
            &LiteralType::List(ref list) => Ok(list.clone()),
            x => Err(UnexpectedVal("List".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_list(&self) -> bool {
        match self {
            &LiteralType::List(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_matrix(&self) -> CalcResult<Matrice<LiteralType>> {
        match self {
            &LiteralType::Matrix(ref matrix) => Ok(matrix.clone()),
            x => Err(UnexpectedVal("List".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_matrix(&self) -> bool {
        match self {
            &LiteralType::Matrix(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_proc(&self) -> CalcResult<(Vec<String>, Expression)> {
        match self {
            &LiteralType::Proc(ref args, ref expr) => Ok((args.clone(), expr.clone())),
            x => Err(UnexpectedVal("LiteralType::Proc".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_proc(&self) -> bool {
        match self {
            &LiteralType::Proc(_, _) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_procedure(&self) -> CalcResult<Procedure> {
        match self {
            &LiteralType::Proc(ref args, ref expr) => Ok(Procedure { params: args.clone(), body: expr.clone() }),
            x => Err(UnexpectedVal("LiteralType::Proc".to_string(), x.to_string()))
        }
    }
    
    #[inline]
    pub fn to_proto(&self) -> CalcResult<ProtoRecord> {
        match self {
            &LiteralType::Proto(ref p) => Ok(p.clone()),
            x => Err(UnexpectedVal("LiteralType::ProtoRecord".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_proto(&self) -> bool {
        match self {
            &LiteralType::Proto(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_structure(&self) -> CalcResult<Record> {
        match self {
            &LiteralType::Structure(ref rec) => Ok(rec.clone()),
            x => Err(UnexpectedVal("LiteralType::Structure".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_structure(&self) -> bool {
        match self {
            &LiteralType::Structure(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_sym_string(&self) -> CalcResult<String> {
        match self {
            &LiteralType::Symbol(ref s) => Ok(s.clone()),
            x => Err(UnexpectedVal("LiteralType::Symbol".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        match self {
            &LiteralType::Symbol(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        match self {
            &LiteralType::Void => true,
            _ => false
        }
    }

    #[inline]
    pub fn recip(&self) -> LitRes {
        match self {
            &LiteralType::BigNum(ref x) => Ok(LiteralType::BigNum(x.recip())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn numer(&'a self) -> CalcResult<&'a BigInt> {
        match self {
            &LiteralType::BigNum(ref x) => Ok(x.numer()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn denom(&'a self) -> CalcResult<&'a BigInt> {
        match self {
            &LiteralType::BigNum(ref x) => Ok(x.denom()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_integer(&self) -> CalcResult<bool> {
        match self {
            &LiteralType::BigNum(ref x) => Ok(x.is_integer()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_even(&self) -> CalcResult<bool> {
        use self::num::integer::Integer;

        match self {
            &LiteralType::BigNum(ref x) => Ok(x.is_integer() && x.numer().is_even()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn abs(self) -> Lit {
        if self < num::zero() {
            -self
        } else {
            self.clone()
        }
    }

    #[inline]
    pub fn floor(&self) -> LitRes {
        match self {
            &LiteralType::BigNum(ref x) => Ok(LiteralType::BigNum(x.floor())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn ceil(&self) -> LitRes {
        match self {
            &LiteralType::BigNum(ref x) => Ok(LiteralType::BigNum(x.ceil())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn round(&self) -> LitRes {
        let one: BigRational = num::one();
        let half = one.clone() / (one.clone() + one);
        let x = try!(self.to_bignum());
        if x > num::zero() {
            Ok(LiteralType::BigNum((x + half).floor()))
        } else {
            Ok(LiteralType::BigNum((x - half).floor()))
        }
    }
}

impl Num for Lit { }

impl Zero for Lit {
    #[inline]
    fn zero() -> Lit {
        LiteralType::BigNum(num::zero())
    }

    #[inline]
    fn is_zero(&self) -> bool {
        match self {
            &LiteralType::BigNum(ref x) => x.is_zero(),
            _ => false
        }
    }
}

impl One for Lit {
    #[inline]
    fn one() -> Lit {
        LiteralType::BigNum(num::one())
    }
}

impl Neg for Lit {
    #[inline]
    type Output = LiteralType;
    fn neg(self) -> Lit {
        match self.clone() {
            LiteralType::BigNum(ref x) => LiteralType::BigNum(-x),
            LiteralType::Matrix(ref x) => LiteralType::Matrix(-(x.clone())),
            _ => panic!("Can't negate something that isn't a number!".to_string())
        }
    }
}

impl Add for Lit {
    #[inline]
    type Output = LiteralType;
    fn add(self, rhs: Lit) -> Lit {
        match (self.clone(), rhs.clone()) {
            (LiteralType::BigNum(ref x), LiteralType::BigNum(ref y)) => LiteralType::BigNum(x + y.clone()),
            (LiteralType::Matrix(ref x), LiteralType::Matrix(ref y)) => LiteralType::Matrix(x.clone() + y.clone()),
            (LiteralType::Matrix(ref x), LiteralType::BigNum(_)) => LiteralType::Matrix(x.scalar(&rhs, |a, b| a + b)),
            (LiteralType::BigNum(_), LiteralType::Matrix(ref x)) => LiteralType::Matrix(x.scalar(&self, |a, b| a + b)),
            _ => panic!(format!("Addition not defined for {} {}", self, rhs))
        }
    }
}

impl CheckedAdd for Lit {
    #[inline]
    fn checked_add(&self, rhs: &Lit) -> Option<Lit> {
        Some(self.clone().add(rhs.clone()))
    }
}

impl Sub for Lit {
    #[inline]
    type Output = LiteralType;
    fn sub(self, rhs: Lit) -> Lit {
        match (self.clone(), rhs.clone()) {
            (LiteralType::BigNum(ref x), LiteralType::BigNum(ref y)) => LiteralType::BigNum(x - y),
            (LiteralType::Matrix(ref x), LiteralType::Matrix(ref y)) => LiteralType::Matrix(x.clone() - y.clone()),
            (LiteralType::Matrix(ref x), LiteralType::BigNum(_)) => LiteralType::Matrix(x.scalar(&rhs, |a, b| a - b)),
            _ => panic!(format!("Subtraction not defined for {} {}", self, rhs))
        }
    }
}

impl Mul for Lit {
    #[inline]
    type Output = LiteralType;
    fn mul(self, rhs: Lit) -> Lit {
        match (self.clone(), rhs.clone()) {
            (LiteralType::BigNum(ref x), LiteralType::BigNum(ref y)) => LiteralType::BigNum(x * y.clone()),
            (LiteralType::Matrix(ref x), LiteralType::BigNum(_)) => LiteralType::Matrix(x.scalar(&rhs, |a, b| a * b)),
            (LiteralType::BigNum(_), LiteralType::Matrix(ref x)) => LiteralType::Matrix(x.scalar(&self, |a, b| a * b)),
            _ => panic!(format!("Multiplication not defined for {} {}", self, rhs))
        }

    }
}

impl Div for Lit {
    #[inline]
    type Output = LiteralType;
    fn div(self, rhs: Lit) -> Lit {
        match (self.clone(), rhs.clone()) {
            (LiteralType::BigNum(ref x), LiteralType::BigNum(ref y)) => LiteralType::BigNum(x / y),
            (LiteralType::Matrix(ref x), LiteralType::BigNum(_)) => LiteralType::Matrix(x.scalar(&rhs, |a, b| a / b)),
             _ => panic!(format!("Division not defined for {} {}", self, rhs))
        }
    }
}

impl Rem for Lit {
    #[inline]
    type Output = LiteralType;
    fn rem(self, rhs: Lit) -> Lit {
        match (self.clone(), rhs.clone()) {
            (LiteralType::BigNum(ref x), LiteralType::BigNum(ref y)) => LiteralType::BigNum(x % y),
            (LiteralType::Matrix(ref x), LiteralType::BigNum(_)) => LiteralType::Matrix(x.scalar(&rhs, |a, b| a % b)),
            _ => panic!(format!("Remainder not defined for {} for {}", self, rhs))
        }
    }
}

impl SquareRoot<Lit> for Lit {
    fn sqrt(&self) -> Lit {
        match self {
            &LiteralType::BigNum(_) => { },
            _ => panic!("Undefined")
        }

        if *self < num::zero() {
            panic!("Undefined")
        }

        let one: Lit = num::one();
        let two = one.clone() + one.clone();
        let sixteen = (two.clone() + two.clone()) * (two.clone() + two.clone());
        let epsilon = self.clone() / (sixteen.clone() * sixteen.clone() * sixteen * two.clone());
        //x / 8192
        
        //Well, I guess it's a guess.
        let mut guess = one.clone();
            
        loop {
            if (self.clone() - guess.clone() * guess.clone()).abs() < epsilon {
                break
            }

            guess = (self.clone() / guess.clone() + guess.clone() ) / (one.clone() + one.clone());
        }

        let rounded = match guess.round() {
            Ok(x) => x,
            Err(f) => panic!(f.to_string())
        };
        if rounded.clone() * rounded.clone() == *self {
            rounded
        } else {
            guess
        }
    }
}
