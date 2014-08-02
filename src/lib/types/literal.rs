//! An enumeration of valid literaltypes

extern crate num;
extern crate matrix;

use self::num::{BigRational, BigInt};
use self::matrix::{Matrice, SquareRoot};
use super::record::{ProtoRecord, Record};
use super::{CalcResult, UnexpectedVal, Expression, Environment};
use std::num;
use std::num::{Zero, One};
use std::fmt;

#[deriving(Clone, PartialEq, PartialOrd, Eq, Ord)]
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

#[deriving(Show, PartialOrd, PartialEq, Eq, Ord, Clone)]
pub struct Procedure {
    params: Vec<String>,
    body: Expression
}

impl<'a> Procedure {
    #[inline]
    pub fn new(params: &Vec<String>, body: &Expression) -> Procedure {
        Procedure { params: params.clone(), body: body.clone() }
    }

    #[inline]
    pub fn to_lit(&self) -> Lit {
        Proc(self.params.clone(), self.body.clone())
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

impl<'a> fmt::Show for WithEnv<'a> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.data {
            &Boolean(ref x) => try!(write!(fmt, "{}", x)),
            &BigNum(ref x) => try!(write!(fmt, "{}", x)),
            &List(ref list) => {
                try!(write!(fmt, "("));
                for elt in list.init().iter() {
                    try!(write!(fmt, "{} ", elt));
                }
                try!(write!(fmt, "{})", match list.last() {
                    Some(x) => x.to_string(),
                    None => "".to_string()
                }));
            },
            &Matrix(ref m) => try!(write!(fmt, "{}", m)),
            &Proc(ref args, ref expr) => {
                try!(write!(fmt, "Procedure: parameters: {}, body: {}", args, expr))
            },
            &Proto(ref p) => try!(write!(fmt, "{}", p)),
            &Structure(ref rec) => try!(write!(fmt, "{}", rec)),
            &Symbol(ref s) => try!(write!(fmt, "{} {}", s, match self.env.lookup(s) {
                Ok(x) => format!("= {}", x),
                Err(m) => m.to_string(),
            })),
            &Void => ()
        }

        Ok(())
    }
}

impl fmt::Show for LiteralType {
    #[inline]   
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Boolean(ref x) => try!(write!(fmt, "{}", x)),
            BigNum(ref x) => try!(write!(fmt, "{}", x)),
            List(ref list) => {
                try!(write!(fmt, "("));
                for elt in list.init().iter() {
                    try!(write!(fmt, "{} ", elt));
                }
                try!(write!(fmt, "{})", match list.last() {
                    Some(x) => x.to_string(),
                    None => "".to_string()
                }));
            }
            Matrix(ref m) => try!(write!(fmt, "{}", m)),
            Structure(ref rec) => try!(write!(fmt, "{}", rec)),
            Proc(ref args, ref expr) => {
                try!(write!(fmt, "Procedure: parameters: {}, body: {}", args, expr))
            },
            Proto(ref p) => try!(write!(fmt, "{}", p)),
            Symbol(ref s) => try!(write!(fmt, "{}", s)),
            Void => ()
        }

        Ok(())
    }
}

impl<'a> LiteralType {
    #[inline]
    pub fn to_bool(&self) -> CalcResult<bool> {
        match self {
            &Boolean(ref b) => Ok(*b),
            x => Err(UnexpectedVal("Boolean".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        match self {
            &Boolean(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_bignum(&self) -> CalcResult<BigRational> {
        match self {
            &BigNum(ref bignum) => Ok(bignum.clone()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_num(&self) -> bool {
        match self {
            &BigNum(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_int(&self) -> CalcResult<int> {
        match self {
            &BigNum(ref bignum) => match bignum.to_integer().to_int() {
                Some(x) => Ok(x),
                None => Err(super::BadArgType("Couldn't convert to int".to_string()))
            },
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn to_uint(&self) -> CalcResult<uint> {
        match self {
            &BigNum(ref bignum) => match bignum.to_integer().to_uint() {
                Some(x) => Ok(x),
                None => Err(super::BadArgType("Couldn't convert to uint".to_string()))
            },
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn to_f64(&self) -> CalcResult<f64> {
        match self {
            &BigNum(ref big) => match (big.numer().to_f64(), big.denom().to_f64()) {
                (Some(x), Some(y)) => Ok((x / y)),
                (_, _) => return Err(super::BadFloatRange)
            },
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }        

    #[inline]
    pub fn to_vec(&self) -> CalcResult<Vec<LiteralType>> {
        match self {
            &List(ref list) => Ok(list.clone()),
            x => Err(UnexpectedVal("List".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_list(&self) -> bool {
        match self {
            &List(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_matrix(&self) -> CalcResult<Matrice<LiteralType>> {
        match self {
            &Matrix(ref matrix) => Ok(matrix.clone()),
            x => Err(UnexpectedVal("List".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_matrix(&self) -> bool {
        match self {
            &Matrix(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_proc(&self) -> CalcResult<(Vec<String>, Expression)> {
        match self {
            &Proc(ref args, ref expr) => Ok((args.clone(), expr.clone())),
            x => Err(UnexpectedVal("Proc".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_proc(&self) -> bool {
        match self {
            &Proc(_, _) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_procedure(&self) -> CalcResult<Procedure> {
        match self {
            &Proc(ref args, ref expr) => Ok(Procedure { params: args.clone(), body: expr.clone() }),
            x => Err(UnexpectedVal("Proc".to_string(), x.to_string()))
        }
    }
    
    #[inline]
    pub fn to_proto(&self) -> CalcResult<ProtoRecord> {
        match self {
            &Proto(ref p) => Ok(p.clone()),
            x => Err(UnexpectedVal("ProtoRecord".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_proto(&self) -> bool {
        match self {
            &Proto(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_structure(&self) -> CalcResult<Record> {
        match self {
            &Structure(ref rec) => Ok(rec.clone()),
            x => Err(UnexpectedVal("Structure".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_structure(&self) -> bool {
        match self {
            &Structure(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn to_sym_string(&self) -> CalcResult<String> {
        match self {
            &Symbol(ref s) => Ok(s.clone()),
            x => Err(UnexpectedVal("Symbol".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        match self {
            &Symbol(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        match self {
            &Void => true,
            _ => false
        }
    }

    #[inline]
    pub fn recip(&self) -> LitRes {
        match self {
            &BigNum(ref x) => Ok(BigNum(x.recip())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn numer(&'a self) -> CalcResult<&'a BigInt> {
        match self {
            &BigNum(ref x) => Ok(x.numer()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn denom(&'a self) -> CalcResult<&'a BigInt> {
        match self {
            &BigNum(ref x) => Ok(x.denom()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_integer(&self) -> CalcResult<bool> {
        match self {
            &BigNum(ref x) => Ok(x.is_integer()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn is_even(&self) -> CalcResult<bool> {
        use self::num::integer::Integer;

        match self {
            &BigNum(ref x) => Ok(x.is_integer() &&x.numer().is_even()),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn abs(&self) -> Lit {
        if *self < num::zero() {
            -self
        } else {
            self.clone()
        }
    }

    #[inline]
    pub fn floor(&self) -> LitRes {
        match self {
            &BigNum(ref x) => Ok(BigNum(x.floor())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn ceil(&self) -> LitRes {
        match self {
            &BigNum(ref x) => Ok(BigNum(x.ceil())),
            x => Err(UnexpectedVal("BigNum".to_string(), x.to_string()))
        }
    }

    #[inline]
    pub fn round(&self) -> LitRes {
        let one: BigRational = num::one();
        let half = one / (one + one);
        let x = try!(self.to_bignum());
        Ok(BigNum((x + half).floor()))
    }
}

pub type Lit = LiteralType;
pub type LitRes =  CalcResult<LiteralType>;

impl Num for Lit { }

impl Zero for Lit {
    #[inline]
    fn zero() -> Lit {
        BigNum(num::zero())
    }

    #[inline]
    fn is_zero(&self) -> bool {
        match self {
            &BigNum(ref x) => x.is_zero(),
            _ => false
        }
    }
}

impl One for Lit {
    #[inline]
    fn one() -> Lit {
        BigNum(num::one())
    }
}

impl Neg<Lit> for Lit {
    #[inline]
    fn neg(&self) -> Lit {
        match self {
            &BigNum(ref x) => BigNum(-x),
            &Matrix(ref x) => Matrix(-x),
            _ => fail!("Can't negate something that isn't a number!".to_string())
        }
    }
}

impl Add<Lit, Lit> for Lit {
    #[inline]
    fn add(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x + *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x + *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a + *b)),
            (&BigNum(_), &Matrix(ref x)) => Matrix(x.scalar(self, |a, b| a + *b)),
            _ => fail!(format!("Addition not defined for {} {}", self, rhs))
        }
    }
}

impl CheckedAdd for Lit {
    #[inline]
    fn checked_add(&self, rhs: &Lit) -> Option<Lit> {
        Some(self.add(rhs))
    }
}

impl Sub<Lit, Lit> for Lit {
    #[inline]
    fn sub(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x - *y),
            (&Matrix(ref x), &Matrix(ref y)) => Matrix(*x - *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a - *b)),
            _ => fail!(format!("Subtraction not defined for {} {}", self, rhs))
        }
    }
}

impl Mul<Lit, Lit> for Lit {
    #[inline]
    fn mul(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x * *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a * *b)),
            (&BigNum(_), &Matrix(ref x)) => Matrix(x.scalar(self, |a, b| a * *b)),
            _ => fail!(format!("Multiplication not defined for {} {}", self, rhs))
        }

    }
}

impl Div<Lit, Lit> for Lit {
    #[inline]
    fn div(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x / *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a / *b)),
             _ => fail!(format!("Division not defined for {} {}", self, rhs))
        }
    }
}

impl Rem<Lit, Lit> for Lit {
    #[inline]
    fn rem(&self, rhs: &Lit) -> Lit {
        match (self, rhs) {
            (&BigNum(ref x), &BigNum(ref y)) => BigNum(x % *y),
            (&Matrix(ref x), &BigNum(_)) => Matrix(x.scalar(rhs, |a, b| a % *b)),
            _ => fail!(format!("Remainder not defined for {} for {}", self, rhs))
        }
    }
}

impl SquareRoot<Lit> for Lit {
    fn sqrt(&self) -> Lit {
        match self {
            &BigNum(_) => { },
            _ => fail!("Undefined")
        }

        if *self < num::zero() {
            fail!("Undefined")
        }

        let one: Lit = num::one();
        let two = one + one;
        let sixteen = (two + two) * (two + two);
        let epsilon = *self / (sixteen * sixteen * sixteen * (one + one));
        //x / 8192
        
        //Well, I guess it's a guess.
        let mut guess = one.clone();
            
        loop {
            if (*self - guess * guess).abs() < epsilon {
                break
            }

            guess = (*self / guess + guess ) / (one + one);
        }

        let rounded = match guess.round() {
            Ok(x) => x,
            Err(f) => fail!(f.to_string())
        };
        if rounded * rounded == *self {
            rounded
        } else {
            guess
        }
    }
}
