#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;
use std::cmp;
use std::num;
use std::iter::AdditiveIterator;

#[cfg(use_fancy)]
use fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};
#[cfg(not(use_fancy))]
use not_fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};

pub mod tensor;
mod tests;

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

impl<T: Num + Clone + fmt::Show> Matrice<T> {
    pub fn rows(&self) -> uint {
        self.rows
    }

    pub fn cols(&self) -> uint {
        self.columns
    }

    pub fn to_vec(&self) -> Vec<T> {
        self.elems.clone()
    }

    pub fn real_len(&self) -> uint {
        self.elems.len()
    }

    pub fn dim_ok(&self) -> bool {
        self.rows * self.columns == self.elems.len()
    }

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

    pub fn append_row(mut self, other: Vec<T>) -> MatrixResult<()> {
        if other.len() != self.columns {
            Err(BadDimensionality)
        } else {
            self.rows = self.rows + 1;
            self.elems.append(other.as_slice());
            Ok(())
        }
    }

    pub fn append_col(&mut self, other: Vec<T>) -> MatrixResult<()> {
        if other.len() != self.rows {
            Err(BadDimensionality)
        } else {
            let mut new_elems = Vec::new();
            for n in range(0, self.rows) {
                new_elems.extend(self.elems.iter().skip(n * self.columns)
                                 .take(self.columns).map(|x| x.clone()));
                new_elems.push(other.get(n).clone());
            }
            self.columns = self.columns + 1;
            self.elems = new_elems;
            Ok(())
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

    pub fn submatrix(&self, ofsx: uint, ofsy: uint, 
                     rows: uint, cols: uint) -> Option<Matrice<T>> {
        if ofsx + cols > self.columns || ofsy + rows > self.rows {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(rows * cols);
        for n in range(ofsy, rows + ofsy) {
            new_elems.extend(self.get_row(n).skip(ofsx).take(cols).map(|x| x.clone()));
        }

        Some(Matrice { columns: cols, rows: rows, elems: new_elems })
    }
    
    pub fn concat_cols(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.rows != other.rows {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(self.rows * 2 * (self.columns
                                                                        + other.columns));
        for n in range(0, self.rows) {
            new_elems.extend(self.get_row(n).take(self.columns).map(|x| x.clone()));
            new_elems.extend(other.get_row(n).take(other.columns).map(|x| x.clone()));
        }
        Some(Matrice { columns: self.columns + other.columns, rows: self.rows,
                       elems: new_elems })
    }

    pub fn concat_rows(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.columns != other.columns {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(self.cols() * 2 * 
                                                       (self.rows + other.rows));
        new_elems.push_all(self.elems.as_slice());
        new_elems.push_all(other.elems.as_slice());
        
        Some(Matrice { columns: self.columns, rows: self.rows + other.rows,
                       elems: new_elems })
    }        

    /// Return an identity matrix of rows & columns n.
    pub fn ident(n: uint) -> Matrice<T> {
        let mut elems: Vec<T> = Vec::from_elem(n * n, num::zero());
        for i in range(0, n) {
            *elems.get_mut(i * n + i) = num::one();
        }
        
        Matrice { rows: n, columns: n, elems: elems }
    }

    /// If the Matrix is square, returns its determinant, otherwise None.
    pub fn determinant(&self) -> Option<T> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        }

        if self.rows == 1 {
            return Some(self.elems.get(0).clone())
        }

        if self.rows == 2 {
            /*             *
             *    |a b|    *
             *    |c d|    *
             * (ad) - (bc) *
             *             */
            Some((*self.elems.get(0) * *self.elems.get(3)) - 
                 (*self.elems.get(1) * *self.elems.get(2)))
        } else {
            let mut sum: T = num::zero();
            for n in range(0, self.columns) { 
                /* Get the element at the top row at the nth position, starting from 0 */
                let elem = self.elems.get(n); 
                
                let next = if n == 0 {
                    /* the submatrix that is in the lower right corner extending
                     * diagonally to element 0 */
                    self.submatrix(1, 1, self.rows - 1, self.columns - 1).unwrap()
                        .determinant().unwrap()
                } else if n == self.columns - 1 {
                    /* the submatrix that is in the lower left corner extending
                     * diagonally to the last element in the first row */ 
                    self.submatrix(0, 1, self.rows - 1, self.columns - 1).unwrap()
                        .determinant().unwrap()
                } else {
                    /* the submatrix that is in the lower left corner, taking up
                     * n columns */
                    let a = self.submatrix(0, 1, self.rows - 1, n).unwrap();

                    /* the submatrix that is in the lower right corner, taking up
                     * the number of columns from n to the number of columns in a row */

                    let b = self.submatrix(n + 1, 1, self.rows - 1, 
                                           self.columns - (n + 1)).unwrap();

                    /* concatenate them and find their determinant */
                    a.concat_cols(&b).unwrap().determinant().unwrap()
                };

                /* If n is even, add the product; if n is odd, subtract the product */
                sum = if n % 2 == 0 {
                    sum + (*elem * next)
                } else {
                    sum - (*elem * next)
                };
            }
            Some(sum)
        }
    }

    /// Returns a new 
    pub fn transpose(&self) -> Matrice<T> {
        if !self.dim_ok() {
            fail!(BadDimensionality.to_str())
        }

        let mut new_elems = Vec::with_capacity(self.real_len());
        for column in range(0, self.columns) {
            new_elems.extend(self.get_col(column).map(|x| x.clone()));
        }

        Matrice { rows: self.columns, columns: self.rows, elems: new_elems }
    }

    /// Returns a matrix of minors.
    pub fn minors(&self) -> Option<Matrice<T>> {
        /* Get a matrix of determinants like so:
         * | a b c |  1. for a: | e f |
         * | d e f |            | h i |
         * | g h i |
         *            2. for b: | d f |
         *                      | g i |
         *            ...
         *            4. for d: | b c |
         *                      | h i |
         *
         *            5. for e: | a c |
         *                      | g i | */
        if self.rows != self.columns || !self.dim_ok() || self.rows == 0 {
            return None
        }

        println!("Input:\n{}", self);

        let mut minors: Vec<T> = Vec::with_capacity(self.rows * self.rows);
        let (subr, subc) = (self.rows - 1, self.columns - 1);
        for row in range(0, self.rows) {
            println!("row {}", row);
            for column in range(0, self.cols()) {
                println!("column {}", column);
                let submatrix = {
                    let a = column + 1; //columns
                    let b = row + 1; //rows
                    /* Top left matrix is displaced 0 rows and 0 columns.
                     * It can extend n rows down, and m columns across. */
                    println!("Calling unwrap on UL matrix!");
                    let topl_matrix = self.submatrix(0, 0, row, column).unwrap();
                    println!("UL matrix:\n{},", topl_matrix);

                    /* Top right matrix is displaced by 0 rows and a columns.
                     * It can extend n rows down and columns - 1 - m columns
                     * across. */
                    println!("Calling unwrap on UR matrix!");
                    let topr_matrix = self.submatrix(b, 0, row, subc - column).unwrap();
                    println!("UR matrix:\n{}", topr_matrix);

                    /* Bottom left matrix is displaced by 0 columns and b rows.
                     * It can extend subr - n rows down and m columns across. */
                    println!("Calling unwrap on LL matrix!");
                    let botl_matrix = self.submatrix(0, b, subr - row, column).unwrap();
                    println!("LL matrix:\n{}", botl_matrix);

                    /* Bottom right matrix is displaced by a columns and b rows.
                     * It can extend subr - n rows down and subc - m columns
                     * across */
                    println!("Calling unwrap on LR matrix!");
                    let botr_matrix = self.submatrix(a, b, subr - row, subc - column).unwrap();
                    println!("LR matrix:\n{}", botr_matrix);

                    println!("Calling unwrap on concatenating upper/lower matrices");
                    let l_matrix = topl_matrix.concat_rows(&botl_matrix).unwrap();
                    println!("Left matrix:\n{}", l_matrix);
                    let r_matrix = topr_matrix.concat_rows(&botr_matrix).unwrap();
                    println!("Right matrix:\n{}", r_matrix);
                    println!("Concatenating the left and right matrices");
                    l_matrix.concat_cols(&r_matrix).unwrap()
                };  
                println!("Pushing\n{}", submatrix);
                minors.push(match submatrix.determinant() {
                    Some(x) => x,
                    None => return None
                });
            }
        }

        Some(Matrice { rows: self.rows, columns: self.columns, elems: minors })
    }

    /// Returns Some(Matrice<T>) if the Matrix has an inverse.
    pub fn inverse(&self) -> Option<Matrice<T>> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        }

        if self.rows == 1 {
            let one: T = num::one();
            return Some( Matrice { rows: 1, columns: 1,
                                   elems: vec!(one / *self.elems.get(0))})
        }
         
        let minors: Vec<T> = match self.minors() {
            Some(x) => x.elems,
            None => return None
        };
        
        let determinant: T = range(0, self.columns).map(|n| {
            if n % 2 == 0 {
                *self.elems.get(n) * *minors.get(n)
            } else {
                -(*self.elems.get(n) * *minors.get(n))
            }
        }).sum();

        let cofactors: Matrice<T> = Matrice { 
            rows: self.rows, columns: self.columns, elems:
            range(0, minors.len())
                .map(|x| if x % 2 == 0 {
                    minors.get(x).clone()
                } else {
                    -*minors.get(x)
                }).collect()
        };
        
        Some(cofactors.transpose().scalar(&determinant, |a, b| *a / *b))
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

impl<T: Num + Clone + fmt::Show> Mul<Matrice<T>, Matrice<T>> for Matrice<T> {
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

impl<T: Num + Clone + fmt::Show> Neg<Matrice<T>> for Matrice<T> {
    fn neg(&self) -> Matrice<T> {
        use std::num;
        let one: T = num::one();
        self.scalar(&-one, |a, b| *a * *b)
    }
}
