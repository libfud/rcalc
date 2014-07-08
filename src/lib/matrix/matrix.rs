#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;
use std::cmp;
use std::iter::AdditiveIterator;

#[cfg(use_fancy)]
use fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};
#[cfg(not(use_fancy))]
use not_fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};

pub mod tensor;

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

pub type MatrixResult<T> = Result<T, MatrixErrors>;

impl fmt::Show for MatrixErrors {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}",  match *self {
            InvalidAxis => "Invalid axis",
            MismatchedAxes => "Mismatched axes",
            BadDimensionality => "Bad dimensionality",
            BadMatrixOp => "Bad matrix operation",
        }));
        Ok(())
    }
}

/* Matrices and their implementation */

#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Matrice<T> {
    columns: uint,
    rows: uint,
    elems: Vec<T>
}

impl<T: fmt::Show > fmt::Show for Matrice<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut columns: Vec<Vec<String>> = Vec::new();
        let mut col_widths: Vec<uint> = Vec::new();

        for column in range(0, self.columns) {
            let mut col = Vec::new();
            for row in range(0, self.rows) {
                col.push(self.elems.get(column + row * self.columns).to_str());
            }
            col_widths.push(col.iter().fold(0, |a, b| cmp::max(a, b.len())));
            columns.push(col);
        }

        for row in range(0, self.rows) {
            try!(write!(fmt, "{}", if row == 0 {
                UpperLeft
            } else if row == self.rows - 1 {
                LowerLeft
            } else {
                MiddleLeft
            }));
            for col in range(0, self.columns) {
                let len = columns.get(col).get(row).len(); // space and comma
                try!(write!(fmt, "{a}{b}{c}", b = columns.get(col).get(row),
                            a = " ".repeat(col_widths.get(col) + 1 - len),
                            c = if col != self.columns - 1 { "," } else { "" }));
            }
            try!(writeln!(fmt, "{}", if row == 0 {
                UpperRight
            } else if row == self.rows - 1 {
                LowerRight
            } else {
                MiddleRight
            }));
        }                

        Ok(())
    }
}

impl<T: Num + Clone> Matrice<T> {
    pub fn new_empty() -> Matrice<T> {
        Matrice { columns: 0, rows: 0, elems: vec![] }
    }

    pub fn new(other: Vec<T>, len: uint, h: uint) -> MatrixResult<Matrice<T>> {
        if len * h != other.len() {
            Err(BadDimensionality)
        } else {
            Ok(Matrice { columns: len, rows: h, elems: other })
        }
    }

    pub fn scalar(&self, rhs: &T, op: |&T, &T| -> T) -> Matrice<T> {
        let new_elems = self.elems.iter().map(|lhs| op(lhs, rhs)).collect();

        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }

    pub fn get_row<'a>(&'a self, row: uint) -> MatrixIterator<'a, T> {
        MatrixIterator { 
            elems: self.elems.slice(row * self.columns, (row + 1) * self.columns),
            jump: 1
        }
    }

    pub fn get_col<'a>(&'a self, col: uint) -> MatrixIterator<'a, T> {
        MatrixIterator {
            elems: self.elems.slice_from(col),
            jump: self.columns
        }
    }

    pub fn ident(n: uint) -> Matrice<T> {
        use std::num;

        let mut elems: Vec<T> = Vec::with_capacity(n * n);
        let zero: T = num::zero();
        let one: T = num::one();
        for x in range(0, n) {
            elems.push_all(range(0, n).map(|y| if y == x { 
                one.clone()
            } else { 
                zero.clone()
            }).collect());
        }
        
        Matrice { rows: n, columns: n, elems: elems }
    }
}

pub struct MatrixIterator<'a, T> {
    elems: &'a [T],
    jump: uint,
}

impl<'a, T> Iterator<&'a T> for MatrixIterator<'a, T> {
    fn next(&mut self) -> Option<&'a T> {
        let val = self.elems.get(0);
        self.elems = self.elems.slice_from(cmp::min(self.jump, self.elems.len()));
        val
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        let size = self.elems.len() / self.jump;
        (size, Some(size))
    }
}

#[test]
fn matrix_empty_test() {
    let empty: Matrice<int> = Matrice::new_empty();
    assert!(empty == Matrice { columns: 0, rows: 0, elems: vec!() });
}

#[test]
fn matrix_new_test() {
    let x = Matrice::new(vec!(1i, 2, 3, 4), 2, 2);
    assert!(x == Ok(Matrice { columns: 2, rows: 2, elems: vec!(1, 2, 3, 4) }));

    let y: MatrixResult<Matrice<int>> = Matrice::new(vec!(), 1, 1);
    assert!(y == Err(BadDimensionality));
}

#[test]
fn matrix_get_row_test() {
    let fake_vec: Vec<int> = range(1, 5).collect();
    let x = match Matrice::new(fake_vec, 2, 2) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };

    let row_0: Vec<int> = x.get_row(0).map(|x| x.clone()).collect();
    assert!(row_0 == vec!(1, 2));

    let row_1: Vec<int> = x.get_row(1).map(|x| x.clone()).collect();
    assert!(row_1 == vec!(3, 4));

    let fake_vec: Vec<int> = range(1, 11).collect();
    let z = match Matrice::new(fake_vec, 2, 5) {
        Ok(x) => x,
        _ => fail!("wut")
    };

    let row_4: Vec<int> = z.get_row(4).map(|x| x.clone()).collect();
    assert!(row_4 == vec!(9, 10));
}

#[test]
fn matrix_get_col_test() {
    let fake_vec: Vec<int> = range(1, 7).collect();
    
    let x = match Matrice::new(fake_vec.clone(), 2, 3) {
        Ok(x) => x,
        Err(_) => fail!("Failed test.")
    };
    
    let col_0: Vec<int> = x.get_col(0).map(|x| x.clone()).collect();
    assert!(col_0 == vec!(1, 3, 5));

    let col_1: Vec<int> = x.get_col(1).map(|x| x.clone()).collect();
    assert!(col_1 == vec!(2, 4, 6));

    let fake_vec: Vec<int> = range(1, 13).collect();
    let z = match Matrice::new(fake_vec, 3, 4) {
        Ok(x) => x,
        _ => fail!("wut")
    };

    let zcol1: Vec<int> = z.get_col(1).map(|x| x.clone()).collect();
    assert!(zcol1 == vec!(2, 5, 8, 11));
}

#[test]
fn ident_test() {
    let x: Matrice<int> = Matrice::ident(1);
    assert!(x == Matrice { rows: 1, columns: 1, elems: vec!(1i) });
}

impl<T: Add<T, T>> Add<Matrice<T>, Matrice<T>> for Matrice<T> {
    fn add(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_str())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs + *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

#[test]
fn test_add() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    assert!(lhs + rhs == Matrice { columns: 2, rows: 2, elems: vec!(6i, 8i, 10i, 12i) });
}

impl<T: Sub<T, T>> Sub<Matrice<T>, Matrice<T>> for Matrice<T> {
    fn sub(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_str())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs - *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

#[test]
fn test_sub() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    assert!(lhs - rhs == Matrice { columns: 2, rows: 2, elems: vec!(-4i, -4i, -4i, -4i) });
}

impl<T: Num + Clone> Mul<Matrice<T>, Matrice<T>> for Matrice<T> {
    fn mul(&self, other: &Matrice<T>) -> Matrice<T> {
        if self.columns != other.rows {
            fail!(MismatchedAxes.to_str())
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(self.columns * other.rows);
        for row in range(0, self.rows) {
            for col in range(0, other.columns) {
                new_elems.push(self.get_row(row).zip(other.get_col(col))
                               .map(|(lhs, rhs)| *lhs * *rhs).sum());
            }
        }

        Matrice { columns: other.columns, rows: self.rows, elems: new_elems }
                
    }
}

#[test]
fn test_mul() {
    let lhs = match Matrice::new(vec!(1i, 2, 3, 4), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };
    let rhs = match Matrice::new(vec!(5i, 6i, 7i, 8i), 2, 2) {
        Ok(x) => x,
        Err(m) => fail!(m.to_str())
    };

    let results = vec!((1i * 5 + 2 * 7), (1 * 6 + 2 * 8),
                       (3 * 5 + 4 * 7), (3 * 6 + 4 * 8));

    assert!(lhs * rhs == Matrice { columns: 2, rows: 2, elems: results });
}

impl<T: Div<T, T>> Div<Matrice<T>, Matrice<T>> for Matrice<T> {
    fn div(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_str())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs / *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Rem<T, T>> Rem<Matrice<T>, Matrice<T>> for Matrice<T> {
    fn rem(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_str())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs % *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Num + Clone> Neg<Matrice<T>> for Matrice<T> {
    fn neg(&self) -> Matrice<T> {
        use std::num;
        let one: T = num::one();
        self.scalar(&-one, |a, b| *a * *b)
    }
}
