#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;
use std::cmp;

#[cfg(use_fancy)]
use fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};
#[cfg(not(use_fancy))]
use not_fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};

#[cfg(use_fancy)]
mod fancy {
    pub static UpperLeft: &'static str = "⎡";
    pub static UpperRight: &'static str = "⎤";
    pub static LowerLeft: &'static str = "⎣";
    pub static LowerRight: &'static str = "⎦";
    pub static MiddleLeft: &'static str = "⎢";
    pub static MiddleRight: &'static str = "⎥";
}

#[cfg(not(use_fancy))]
mod not_fancy {
    pub static UpperLeft: &'static str = "[";
    pub static UpperRight: &'static str = "]";
    pub static LowerLeft: &'static str = "[";
    pub static LowerRight: &'static str = "]";
    pub static MiddleLeft:  &'static str = "|";
    pub static MiddleRight: &'static str = "|";
}

#[deriving(Clone, PartialEq)]
pub enum MatrixErrors {
    InvalidAxis,
    MismatchedAxes, 
    BadDimensionality,
    BadMatrixOp
}

impl fmt::Show for MatrixErrors {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        print!("{}",  match *self {
            InvalidAxis => "Invalid axis",
            MismatchedAxes => "Mismatched axes",
            BadDimensionality => "Bad dimensionality",
            BadMatrixOp => "Bad matrix operation",
        });
        Ok(())
    }
}

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Axis {
    X,
    Y,
    Z,
    T
}

impl Axis {
    pub fn from_str(s: &String) -> MatrixResult<Axis> {
        let res = match s.as_slice() {
            "X" => X,
            "Y" => Y,
            "Z" => Z, 
            "T" => T,
            _ => return Err(InvalidAxis)
        };
        Ok(res)
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum Dimensionality {
    Null,
    OneD(Axis, uint),
    TwoD((Axis, uint), (Axis, uint)),
    ThreeD((Axis, uint), (Axis, uint), (Axis, uint)),
    FourD((Axis, uint), (Axis, uint), (Axis, uint), (Axis, uint)),
}

impl Dimensionality {
    pub fn get_lens(&self) -> Vec<uint> {
        match self {
            &Null => vec![],
            &OneD(_, x) => vec!(x),
            &TwoD((_, x), (_, y)) => vec!(x, y),
            &ThreeD((_, x), (_, y), (_, z)) => vec!(x, y, z),
            &FourD((_, x), (_, y), (_, z), (_, t)) => vec!(x, y, z, t),
        }
    }

    pub fn get_axes(&self) -> Vec<Axis> {
        match self {
            &Null => vec![],
            &OneD(x, _) => vec!(x),
            &TwoD((x, _), (y, _)) => vec!(x, y),
            &ThreeD((x, _), (y, _), (z, _)) => vec!(x, y, z),
            &FourD((x, _), (y, _), (z, _), (t, _)) => vec!(x, y, z, t),
        }
    }

    pub fn get_dim(dimens: (Vec<(Axis, uint)>)) -> MatrixResult<Dimensionality> {
        match dimens.len() {
            0 => Ok(Null),
            1 => {
                let (x, len) = dimens.get(0).clone();
                Ok(OneD(x, len))
            },
            2 => {
                let (x, len) = dimens.get(0).clone();
                let (y, wid) = dimens.get(1).clone();
                if x == y {
                    return Err(BadDimensionality)
                }
                Ok(TwoD((x, len), (y, wid)))
            }
            3 => {
                let (x, len) = dimens.get(0).clone();
                let (y, wid) = dimens.get(1).clone();
                let (z, h) = dimens.get(2).clone();
                if x == y || x == z || y == z {
                    return Err(BadDimensionality)
                }
                Ok(ThreeD((x, len), (y, wid), (z, h)))
            }
            4 => {
                let (x, len) = dimens.get(0).clone();
                let (y, wid) = dimens.get(1).clone();
                let (z, h) = dimens.get(2).clone();
                let (t, s) = dimens.get(3).clone();
                if x == y || x == z || x == t || y == z || y == t || z == t {
                    return Err(BadDimensionality)
                }
                Ok(FourD((x, len), (y, wid), (z, h), (t, s)))
            }
            _ => Err(BadDimensionality)
        }
    }
}

impl fmt::Show for Dimensionality {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        let axes = self.get_axes();
        let dimens = self.get_lens();
        for (a, b) in axes.iter().zip(dimens.iter()) {
            print!("{} : {} ", a, b);
        }
        Ok(())
    }
}

pub type Dim = Dimensionality;

pub type MatrixResult<T> = Result<T, MatrixErrors>;

#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Tensor<T> {
    dimensionality: Dimensionality,
    elems: Vec<T>,
}

impl<T: fmt::Show > fmt::Show for Tensor<T> {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!("{}", self.dimensionality);
        let lens = self.dimensionality.get_lens();
        match self.dimensionality {
            Null => { },
            OneD(_, _) => println!("{}", self.elems),
            TwoD(_, _) => {
                let (a, b) = (lens.get(0), lens.get(1));
                for column in range(0, *a) {
                    println!("{} ", self.elems.slice(column * *b, column * *b + *b));
                }
            }
            _ => println!("I don't know yet.\n{}", self.elems),
        }
        Ok(())
    }
}            

pub trait FakeNum<T, U>: Add<T, Result<T, U>> + 
    Sub<T, Result<T, U>> + Mul<T, Result<T, U>> + 
    Div<T, Result<T, U>> + Rem<T, Result<T, U>> { }

impl<T: Clone + FakeNum<T, U>, U> Tensor<T> {
    pub fn new_null() -> Tensor<T> {
        Tensor { dimensionality: Null, elems: vec![] }
    }

    pub fn new_empty(axes: Vec<Axis>) -> MatrixResult<Tensor<T>> {
        if axes.len() > 4 {
            return Err(BadDimensionality)
        }
        
        if axes.len() > 0 {
            let mut a = axes.get(0);
            for axis in axes.tail().iter() {
                if a == axis {
                    return Err(MismatchedAxes)
                }
                a = axis;
            }
        }

        let e: Vec<T> = vec![];
        let dimens: Vec<(Axis, uint)> = axes.move_iter().map(|x| (x, 0)).collect();
        Ok(Tensor { dimensionality: try!(Dimensionality::get_dim(dimens)), elems: e })
    }

    pub fn new(other: Vec<T>, other_dim: Vec<(Axis, uint)>) -> MatrixResult<Tensor<T>> {
        let total_len = other_dim.iter().fold(1, |a, &b| a * match b { (_, x) => x });
        if other.len() != total_len {
            return Err(BadDimensionality)
        }

        let dimens = try!(Dimensionality::get_dim(other_dim));
        
        Ok(Tensor { dimensionality : dimens, elems: other })
    }

    pub fn scalar<U>(&self, scalar: &T, 
                     op: |&T, &T| -> Result<T, U>) -> MatrixResult<Tensor<T>> {
        let mut elems = Vec::new();
        for x in self.elems.iter() {
            let res = op(x, scalar);
            match res {
                Ok(x) => elems.push(x),
                Err(_) => return Err(BadMatrixOp)
            }
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: elems })
    }

    pub fn set(&mut self, other: Vec<T>, other_dim: Dim) -> MatrixResult<()> {
        let lens = other_dim.get_lens();
        let total_len = lens.iter().fold(1u, |a, &b| a * b);
        if total_len != other.len() {
            return Err(BadDimensionality)
        }

        self.dimensionality = other_dim;
        self.elems = other;

        Ok(())
    }
    
    pub fn extend_1d(&mut self, other: Vec<T>) -> MatrixResult<()> {
        let axes = self.dimensionality.get_axes();

        let axis = match axes.len() {
            1 => axes.get(0).clone(),
            _ => return Err(MismatchedAxes)
        };
            
        self.elems.push_all(other.as_slice());
        let len = self.elems.len();

        self.dimensionality = OneD(axis, len);
        Ok(())
    }

    pub fn extend_2d(&mut self, other: Vec<T>, other_dim: Dim) -> MatrixResult<()> {
        let scalars = self.dimensionality.get_lens();
        let (length, width) = match scalars.len() {
            2 => (scalars.get(0).clone(), scalars.get(1).clone()),
            _ => return Err(BadDimensionality)
        };

        let other_scalars = other_dim.get_lens();
        let (other_len, other_wid) = match other_scalars.len() {
            2 => (other_scalars.get(0).clone(), other_scalars.get(1).clone()),
            _ => return Err(BadDimensionality),
        };

        let axes = self.dimensionality.get_axes();
        let (a, b) = match axes.len() {
            2 => (axes.get(0), axes.get(1)),
            _ => return Err(BadDimensionality),
        };


        let other_axes = other_dim.get_axes();
        let (x, y) = match other_axes.len() {
            2 => (other_axes.get(0), other_axes.get(1)),
            _ => return Err(BadDimensionality),
        };

        if (a != x) || (b != y) {
            return Err(MismatchedAxes)
        }
        
        if other_wid != width || other_len * other_wid != other.len() {
            return Err(MismatchedAxes)
        }

        let new_len = length + other_len;
        let mut new_elems = Vec::new();
        for column in range(0, width) {
            new_elems.push_all(self.elems.slice(column * length, column * length + length));
            new_elems.push_all(other.slice(column * other_len, column * other_len + other_len));
        }

        self.dimensionality = try!(Dimensionality::get_dim(vec!((x.clone(), new_len),
                                                                (y.clone(), width))));
        self.elems = new_elems;
        Ok(())
    }
}
 
impl<T: Add<T, Result<T, U>>, U> Add<Tensor<T>, MatrixResult<Tensor<T>>> for Tensor<T> {
    fn add(&self, other: &Tensor<T>) -> MatrixResult<Tensor<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a + *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Sub<T, Result<T, U>>, U> Sub<Tensor<T>, MatrixResult<Tensor<T>>> for Tensor<T> {
    fn sub(&self, other: &Tensor<T>) -> MatrixResult<Tensor<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a - *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Mul<T, Result<T, U>>, U> Mul<Tensor<T>, MatrixResult<Tensor<T>>> for Tensor<T> {
    fn mul(&self, other: &Tensor<T>) -> MatrixResult<Tensor<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a * *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Div<T, Result<T, U>>, U> Div<Tensor<T>, MatrixResult<Tensor<T>>> for Tensor<T> {
    fn div(&self, other: &Tensor<T>) -> MatrixResult<Tensor<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a / *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Rem<T, Result<T, U>>, U> Rem<Tensor<T>, MatrixResult<Tensor<T>>> for Tensor<T> {
    fn rem(&self, other: &Tensor<T>) -> MatrixResult<Tensor<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }

        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a % *b {
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

/* Matrices and their implementation */

#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Matrice<T> {
    length: uint,
    height: uint,
    elems: Vec<T>
}

impl<T: fmt::Show > fmt::Show for Matrice<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut columns: Vec<Vec<String>> = Vec::new();
        let mut col_widths: Vec<uint> = Vec::new();

        for column in range(0, self.length) {
            let mut col = Vec::new();
            for row in range(0, self.height) {
                col.push(self.elems.get(column + row * self.length).to_str());
            }
            col_widths.push(col.iter().fold(0, |a, b| cmp::max(a, b.len())));
            columns.push(col);
        }

        for row in range(0, self.height) {
            try!(write!(fmt, "{}", if row == 0 {
                UpperLeft
            } else if row == self.height - 1 {
                LowerLeft
            } else {
                MiddleLeft
            }));
            for col in range(0, self.length) {
                let len = columns.get(col).get(row).len(); // space and comma
                try!(write!(fmt, "{a}{b}{c}", b = columns.get(col).get(row),
                            a = " ".repeat(col_widths.get(col) + 1 - len),
                            c = if col != self.length - 1 { "," } else { "" }));
            }
            try!(writeln!(fmt, "{}", if row == 0 {
                UpperRight
            } else if row == self.height - 1 {
                LowerRight
            } else {
                MiddleRight
            }));
        }                

        Ok(())
    }
}

impl<T: FakeNum<T, U> + Clone, U> Matrice<T> {
    pub fn new_empty() -> Matrice<T> {
        Matrice { length: 0, height: 0, elems: vec![] }
    }

    pub fn new(other: Vec<T>, len: uint, h: uint) -> MatrixResult<Matrice<T>> {
        if len * h != other.len() {
            Err(BadDimensionality)
        } else {
            Ok(Matrice { length: len, height: h, elems: other })
        }
    }

    pub fn get_row<'a>(&'a self, row: uint) -> Option<&'a [T]> {
        if row >= self.height {
            return None
        }

        Some(self.elems.slice(row * self.length, row * self.length + self.length))
    }

    pub fn get_col<'a>(&'a self, col: uint) -> Option<Vec<&'a T>> {
        if col >= self.length {
            return None
        }

        let mut column: Vec<&'a T> = Vec::with_capacity(self.height);
        for row in range(0u, self.height) {
            column.push(self.elems.get(row * self.length + col));
        }

        Some(column)
    }
}

#[deriving(Clone, Show, PartialEq)]
struct Fake {
    fake: int
}

impl Fake { pub fn new(x: int) -> Fake { Fake { fake: x } } }

impl FakeNum<Fake, Fake> for Fake { }

impl Add<Fake, Result<Fake, Fake>> for Fake {
    fn add(&self, rhs: &Fake) -> Result<Fake, Fake> { Ok(Fake::new(self.fake + rhs.fake)) }
}
impl Sub<Fake, Result<Fake, Fake>> for Fake {
    fn sub(&self, rhs: &Fake) -> Result<Fake, Fake> { Ok(Fake::new(self.fake - rhs.fake)) }
}
impl Mul<Fake, Result<Fake, Fake>> for Fake {
    fn mul(&self, rhs: &Fake) -> Result<Fake, Fake> { Ok( Fake::new(self.fake * rhs.fake)) }
}
impl Div<Fake, Result<Fake, Fake>> for Fake {
    fn div(&self, rhs: &Fake) -> Result<Fake, Fake> { 
        if rhs.fake != 0 {
            Ok( Fake::new(self.fake / rhs.fake))
        } else {
            Err(Fake::new(0))
        }
    }
}
impl Rem<Fake, Result<Fake, Fake>> for Fake {
    fn rem(&self, rhs: &Fake) -> Result<Fake, Fake> { 
        if rhs.fake != 0 {
            Ok(Fake::new(self.fake / rhs.fake))
        } else {
            Err(Fake::new(0))
        }
    }
}

#[test]
fn matrix_empty_test() {
    let empty: Matrice<Fake> = Matrice::new_empty();
    assert!(empty == Matrice { length: 0, height: 0, elems: vec!() });
}

#[test]
fn matrix_new_test() {
    let (f1, f2, f3, f4) = (Fake::new(1), Fake::new(2), Fake::new(3),Fake::new(4));
    let (g1, g2, g3, g4) = (f1.clone(), f2.clone(), f3.clone(), f4.clone());

    let x = Matrice::new(vec!(f1, f2, f3, f4), 2, 2);
    assert!(x == Ok(Matrice { length: 2, height: 2, elems: vec!(g1, g2, g3, g4) }));
    let y: MatrixResult<Matrice<Fake>> = Matrice::new(vec!(), 1, 1);
    assert!(y == Err(BadDimensionality));
}

#[test]
fn matrix_get_row_test() {
    let (f1, f2, f3, f4) = (Fake::new(1), Fake::new(2), Fake::new(3), Fake::new(4));
    let fake_vec = vec!(f1.clone(), f2.clone(), f3.clone(), f4.clone());
    let x = match Matrice::new(fake_vec.clone(), 2, 2) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };
    assert!(x.get_row(0) == Some(&[f1, f2]));
    assert!(x.get_row(1) == Some(&[f3, f4]));
    assert!(x.get_row(2) == None);

    let fake_vec: Vec<Fake> = range(1, 11).map(|x| Fake::new(x)).collect();
    let z = match Matrice::new(fake_vec, 2, 5) {
        Ok(x) => x,
        _ => fail!("wut")
    };
    let row4 = vec!(Fake::new(9), Fake::new(10));
    assert!(z.get_row(4) == Some(row4.as_slice()));
}

#[test]
fn matrix_get_col_test() {
    let fake_vec = vec! (Fake::new(1), Fake::new(2), 
                                    Fake::new(3), Fake::new(4),
                                    Fake::new(5), Fake::new(6));
    
    let x = match Matrice::new(fake_vec.clone(), 2, 3) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };

    let (f1, f2, f3, f4, f5, f6) = (Fake::new(1), Fake::new(2), 
                                    Fake::new(3), Fake::new(4),
                                    Fake::new(5), Fake::new(6));
    
    assert!(x.get_col(0) != Some(vec!(&f1, &f2, &f3)));
    println!("{}", x.get_col(0));
    assert!(x.get_col(0) == Some(vec!(&f1, &f3, &f5)));
    assert!(x.get_col(1) == Some(vec!(&f2, &f4, &f6)));
    assert!(x.get_col(2) == None);

    let fake_vec: Vec<Fake> = range(1, 13).map(|x| Fake::new(x)).collect();
    let z = match Matrice::new(fake_vec, 3, 4) {
        Ok(x) => x,
        _ => fail!("wut")
    };
    let (f8, f11) = (Fake::new(8), Fake::new(11));
    assert!(z.get_col(1).unwrap() == vec!(&f2, &f5, &f8, &f11));
}
 
impl<T: Add<T, Result<T, U>>, U> Add<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn add(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if (self.length, self.height) != (other.length, other.height) {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a + *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { length: self.length, height: self.height, elems: new_elems })
    }
}

impl<T: Sub<T, Result<T, U>>, U> Sub<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn sub(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if (self.length, self.height) != (other.length, other.height) {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a - *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { length: self.length, height: self.height, elems: new_elems })
    }
}

impl<T: Mul<T, Result<T, U>>, U> Mul<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn mul(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if (self.length, self.height) != (other.length, other.height) {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a * *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { length: self.length, height: self.height, elems: new_elems })
/*
        if self.length != other.height {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for column in range(0, self.length) {
            let mut resu
            for column in range(column, other.height) {
  */          
    }
}

impl<T: Div<T, Result<T, U>>, U> Div<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn div(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if (self.length, self.height) != (other.length, other.height) {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a / *b { 
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { length: self.length, height: self.height, elems: new_elems })
    }
}

impl<T: Rem<T, Result<T, U>>, U> Rem<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn rem(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if (self.length, self.height) != (other.length, other.height) {
            return Err(MismatchedAxes)
        }

        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push(match *a % *b {
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { length: self.length, height: self.height, elems: new_elems })
    }
}

