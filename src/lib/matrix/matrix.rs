#![crate_name = "matrix"]
#![crate_type = "lib"]

use std::fmt;
use std::cmp;
use std::num;
use std::mem;
use std::iter::AdditiveIterator;

#[cfg(use_fancy)]
use fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};
#[cfg(not(use_fancy))]
use not_fancy::{UpperLeft, UpperRight, LowerLeft, LowerRight, MiddleLeft, MiddleRight};

pub mod tensor;

#[cfg(test)]
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
    #[inline]
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

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub struct Matrice<T> {
    columns: uint,
    rows: uint,
    elems: Vec<T>
}

impl<T: fmt::Show > fmt::Show for Matrice<T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut columns: Vec<Vec<String>> = Vec::new();
        let mut col_widths: Vec<uint> = Vec::new();

        for column in range(0, self.columns) {
            let mut col = Vec::new();
            for row in range(0, self.rows) {
                col.push(self.elems[column + row * self.columns].to_string());
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
                let len = columns[col][row].len(); // space and comma
                try!(write!(fmt, "{a}{b}{c}", b = columns[col][row],
                            a = " ".repeat(col_widths[col] + 1 - len),
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

impl<T> Matrice<T> {
    #[inline]
    pub fn new() -> Matrice<T> {
        Matrice { columns: 0, rows: 0, elems: vec![] }
    }

    #[inline]
    pub fn scalar(&self, rhs: &T, op: |&T, &T| -> T) -> Matrice<T> {
        let new_elems = self.elems.iter().map(|lhs| op(lhs, rhs)).collect();

        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }

    #[inline]
    pub fn rows(&self) -> uint {
        self.rows
    }

    #[inline]
    pub fn cols(&self) -> uint {
        self.columns
    }

    #[inline]
    pub fn real_len(&self) -> uint {
        self.elems.len()
    }

    #[inline]
    pub fn get_row<'a>(&'a self, row: uint) -> MatriceIterator<'a, T> {
        MatriceIterator { 
            elems: self.elems.slice(row * self.columns, (row + 1) * self.columns),
            jump: 1
        }
    }

    #[inline]
    pub fn get_col<'a>(&'a self, col: uint) -> MatriceIterator<'a, T> {
        MatriceIterator {
            elems: self.elems.slice_from(col),
            jump: self.columns
        }
    }

    #[inline]
    pub fn left_diag<'a>(&'a self) -> MatriceIterator<'a, T> {
        MatriceIterator {
            elems: self.elems.as_slice(),
            jump: self.columns + 1
        }
    }

    #[inline]
    pub fn right_diag<'a>(&'a self) -> MatriceIterator<'a, T> {
        MatriceIterator {
            elems: self.elems.slice(self.columns - 1,
                                    (self.rows - 1) * self.columns + 1),
            jump: self.columns - 1
        }
    }

    #[inline]
    pub fn swap_rows(&mut self, row_a: uint, row_b: uint) -> MatrixResult<()> {
        if row_a == row_b {
            return Ok(())
        } else if row_a > self.rows || row_b > self.rows {
            return Err(BadDimensionality)
        }

        let (small, big) = (cmp::min(row_a, row_b), cmp::max(row_a, row_b));

        let cols = self.columns;

        let (low, high ) = self.elems.mut_split_at(big * cols);
        let low = low.mut_slice(small * cols, (small + 1) * cols);
        let high = high.mut_slice_to(cols);

        for (low_elem, high_elem) in low.mut_iter().zip(high.mut_iter()) {
            mem::swap(low_elem, high_elem);
        }

        Ok(())
    }
}

impl<T: Clone> Matrice<T> {
    #[inline]
    pub fn from_matrix<'a>(matrix: &'a Matrice<T>) -> Matrice<T> {
        Matrice {
            rows: matrix.rows, columns: matrix.columns,
            elems: matrix.elems.iter().map(|e| e.clone()).collect(),
        }
    }

    #[inline]
    pub fn from_vec(other: Vec<T>, width: uint, height: uint) -> MatrixResult<Matrice<T>> {
        if width * height != other.len() {
            Err(BadDimensionality)
        } else {
            Ok(Matrice { columns: width, rows: height, elems: other })
        }
    }

    #[inline]
    pub fn get_elem(&self, row: uint, column: uint) -> Option<T> {
        if row * self.columns + column > self.elems.len() {
            None
        } else {
            Some(self.elems[row * self.columns + column].clone())
        }
    }

    #[inline]
    pub fn append_row(&mut self, other: Vec<T>) -> MatrixResult<()> {
        if other.len() != self.columns {
            Err(BadDimensionality)
        } else {
            self.rows = self.rows + 1;
            let mut new_elems = self.elems.clone();
            new_elems = new_elems.append(other.as_slice());
            self.elems = new_elems;
            Ok(())
        }
    }

    #[inline]
    pub fn append_col(&mut self, other: Vec<T>) -> MatrixResult<()> {
        if other.len() != self.rows {
            Err(BadDimensionality)
        } else {
            let mut new_elems = Vec::new();
            for n in range(0, self.rows) {
                new_elems.extend(self.elems.iter().skip(n * self.columns)
                                 .take(self.columns).map(|x| x.clone()));
                new_elems.push(other[n].clone());
            }
            self.columns = self.columns + 1;
            self.elems = new_elems;
            Ok(())
        }
    }

    #[inline]
    pub fn to_vec(&self) -> Vec<T> {
        self.elems.clone()
    }

    #[inline]
    pub fn submatrix(&self, ofsx: uint, ofsy: uint, 
                     rows: uint, cols: uint) -> Option<Matrice<T>> {
        if ofsx + cols > self.columns || ofsy + rows > self.rows {
            println!("{} + {} = {}. Columns = {}. {} + {} = {}. Rows = {}",
                     ofsx, cols, ofsx + cols, self.columns,
                     ofsy, rows, ofsy + rows, self.rows);
            println!("Returning none from submatrix");
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(rows * cols);
        for n in range(ofsy, rows + ofsy) {
            new_elems.extend(self.get_row(n).skip(ofsx).take(cols).map(|x| x.clone()));
        }

        Some(Matrice { columns: cols, rows: rows, elems: new_elems })
    }
    
    #[inline]
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

    #[inline]
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

    #[inline]
    pub fn set_row(&mut self, old_row: uint, new_row: Vec<T>) -> MatrixResult<()> {
        if old_row > self.rows || new_row.len() > self.columns {
            return Err(BadDimensionality)
        }

        for (elt, new) in range(old_row * self.columns,
                                (old_row + 1) * self.columns).zip(new_row.iter()) {
            *self.elems.get_mut(elt) = new.clone()
        }

        Ok(())
    }

    #[inline]
    pub fn set_col(&mut self, old_col: uint, new_col: Vec<T>) -> MatrixResult<()> {
        if old_col > self.columns || new_col.len() > self.rows {
            return Err(BadDimensionality)
        }

        for (elt, new) in range(0, self.rows).zip(new_col.iter()) {
            *self.elems.get_mut(elt * self.rows + old_col) = new.clone()
        }
        
        Ok(())
    }

    #[inline]
    pub fn decross(&self, omit_row: uint, omit_col: uint) -> Matrice<T> {
        let mut new_elems: Vec<T> = Vec::new();
        for row in range(0, self.rows) {
            if row + 1 == omit_row {
                continue
            }
            for col in range(0, self.columns) {
                if col + 1 == omit_col {
                    continue
                } 
                new_elems.push(self.elems[row * self.columns + col].clone());
            }
        }

        let new_rows = if omit_row == 0 || omit_row > self.rows {
            self.rows
        } else {
            self.rows - 1
        };
        
        let new_cols = if omit_col == 0 || omit_col > self.columns {
            self.columns
        } else {
            self.columns - 1
        };

        Matrice { rows: new_rows, columns: new_cols, elems: new_elems }
    }
}

impl<T: Num> Matrice<T> {
    /// Return an identity matrix of rows & columns n.
    #[inline]
    pub fn ident(n: uint) -> Matrice<T> {
        let mut elems: Vec<T> = Vec::from_fn(n * n, |_| num::zero());
        for i in range(0, n) {
            *elems.get_mut(i * n + i) = num::one();
        }
        
        Matrice { rows: n, columns: n, elems: elems }
    }

    #[inline]
    pub fn get_pivot<'a, It: Iterator<&'a T>>(iterator: It) -> Option<(&'a T, uint)> {
        iterator.enumerate().find(|&(_, x)| *x != num::zero())
            .map(|(idx, x)| (x, idx))
    }

    /// Return a vector of pivots and the column at which they occur.
    #[inline]
    pub fn row_pivots<'a>(&'a self) -> Vec<Option<(&'a T, uint)>> {
        range(0, self.rows).map(|row| Matrice::get_pivot(self.get_row(row))).collect()
    }

    #[inline]
    pub fn col_pivots<'a>(&'a self) -> Vec<Option<(&'a T, uint)>> {
        range(0, self.columns).map(|col| Matrice::get_pivot(self.get_col(col))).collect()
    }

    #[inline]
    pub fn is_ident(&self) -> bool {
        if self.rows != self.columns {
            false
        } else {
            let one: T = num::one();
            range(0, self.rows()).all(|i| {
                self.get_row(i).enumerate().all(|(j, x)| if i == j {
                    *x == one 
                } else { 
                    x.is_zero()
                })
            })
        }
    }
}

impl<T: Num + PartialOrd + Clone + fmt::Show> Matrice<T> {
    #[inline]
    pub fn trace(&self) -> Option<T> {
        if self.rows != self.columns {
            None
        } else {
            let zero: T = num::zero();
            Some(self.left_diag().fold(zero, |a, b| a + *b))
        }
    }
    
    #[inline]
    pub fn hadamard_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.rows != other.rows || self.columns != other.columns {
            None
        } else {
            Some(Matrice { rows: self.rows, columns: self.columns,
                           elems: self.elems.iter().zip(other.elems.iter()).map(|(x, y)| {
                               *x * *y}).collect() })
        }
    }

    #[inline]
    pub fn kronecker_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.columns != other.rows {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(self.columns * other.rows);
        for row in range(0, self.rows) {
            for col in range(0, other.columns) {
                new_elems.push(self.get_row(row).zip(other.get_col(col))
                               .map(|(lhs, rhs)| *lhs * *rhs).sum());
            }
        }

        Some(Matrice { columns: other.columns, rows: self.rows, elems: new_elems })
    }

    #[inline]
    pub fn determinant(&self) -> Option<T> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        } else if self.rows == 1 {
            return Some(self.elems[0].clone())
        } else if self.rows == 2 {
            /* ad - bc */
            return Some((self.elems[0] * self.elems[3]) - 
                        (self.elems[1] * self.elems[2]))
        }

        if self.is_ident() {
            return Some(num::one())
        }

        let mut upper = self.clone();

        /* We're going to change the tail rows so that we get into row echelon form. */
        let mut swaps = 0u;
        for row in range(0, self.rows) {
            /* Because the final determinant is equal to the product
             * of the main diagonal, it doesn't matter if we switch rows
             * to avoid division by zero. */
            let p_row = match Matrice::get_pivot(upper.get_col(row).skip(row)) {
                Some((_, r)) => r + row,
                None => continue
            };

            if p_row > row {
                swaps += 1;
                match upper.swap_rows(row, p_row) {
                    Ok(()) => { }
                    Err(m) => fail!(m.to_string())
                }
            }

            /* We're working to zero out those columns appropiately, so for a 3x3, 
             * so the divisors will be along the main diagonal */
            let divisor = upper.elems[row * self.columns + row].clone();

            for next_row in range(row + 1, self.rows) {
                let n_row = next_row * self.columns;
                let lower_elt = upper.elems[n_row + row] / divisor;

                let new_row: Vec<T> = upper.get_row(next_row).zip(
                    upper.get_row(row))
                    .map(|(x, y)| *x - (*y * lower_elt)).collect();

                match upper.set_row(next_row, new_row) {
                    Ok(_) => { },
                    Err(_) => return None
                }
            }
        }

        let one: T = num::one();
        let prod = upper.left_diag().fold(one, |a, b| a * *b);

        if swaps % 2 == 0 {
            Some(prod)
        } else {
            Some(-prod)
        }
    }

    /// Passing zero as an argument for row and column returns the same
    /// matrix. Passing arguments greater than the number of rows & columns
    /// also returns the same matrix.
    pub fn sans_row_col<'a>(&'a self, omit_row: uint, omit_col: uint) -> Matrice<&'a T> {
        let mut new_elems: Vec<&'a T> = Vec::new();
        for row in range(0, self.rows) {
            if row + 1 == omit_row {
                continue
            }
            for col in range(0, self.columns) {
                if col + 1 == omit_col {
                    continue
                } 
                new_elems.push(&self.elems[row * self.columns + col]);
            }
        }

        let new_rows = if omit_row == 0 || omit_row > self.rows {
            self.rows
        } else {
            self.rows - 1
        };
        
        let new_cols = if omit_col == 0 || omit_col > self.columns {
            self.columns
        } else {
            self.columns - 1
        };

        Matrice { rows: new_rows, columns: new_cols, elems: new_elems }
    }

    #[inline]
    pub fn transpose(&self) -> Matrice<T> {
        let mut new_elems = Vec::with_capacity(self.elems.len());
        for column in range(0, self.columns) {
            new_elems.extend(self.get_col(column).map(|x| x.clone()));
        }

        Matrice { rows: self.columns, columns: self.rows, elems: new_elems }
    }

    #[inline]
    pub fn translate_by(&self, other: &Vec<T>) -> Option<Matrice<T>> {
        if other.len() != self.rows { 
            return None
        }

        let mut new_elems = Vec::with_capacity(self.elems.len());

        for row in range(0, self.rows) {
            new_elems.extend(self.get_row(row).map(|x| *x + other[row]))
        }

        Some(Matrice { rows: self.rows, columns: self.rows, elems: new_elems })
    }

    /// Returns a matrix of minors.
    #[inline]
    pub fn minors(&self) -> Option<Matrice<T>> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        }

        let mut minors: Vec<T> = Vec::with_capacity(self.rows * self.rows);
        for row in range(0, self.rows) {
            for column in range(0, self.cols()) {
                let submatrix = self.decross(row + 1, column + 1);
                minors.push(match submatrix.determinant() {
                    Some(x) => x,
                    None => return None
                });
            }
        }

        Some(Matrice { rows: self.rows, columns: self.columns, elems: minors })
    }

    /// Returns Some(Matrice<T>) if the Matrice has an inverse.
    pub fn inverse(&self) -> Option<Matrice<T>> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        } else if self.is_ident() {
            return Some(self.clone())
        } else if self.rows == 1 {
            let one: T = num::one();
            return Some( Matrice { rows: 1, columns: 1,
                                   elems: vec!(one / self.elems[0])})
        }

        let mut minors = match self.minors() {
            Some(x) => x,
            None => return None
        };

        let determinant: T = range(0, self.columns).map(|n| {
            if n % 2 == 0 {
                self.elems[n] * minors.elems[n]
            } else {
                -(self.elems[n] * minors.elems[n])
            }
        }).sum();

        if determinant.is_zero() {
            return None
        }

        for row in range(0, minors.rows) {
            for x in range(0, minors.columns) {
                let i = row * self.cols() + x;
                if (row % 2 == 0)^(x % 2 == 0) {
                    *minors.elems.get_mut(i) = minors.elems[i] / -determinant
                } else {
                    *minors.elems.get_mut(i) = minors.elems[i] / determinant
                }
            }
        }
        
        Some(minors.transpose())
    }
}

impl<T: Num + PartialOrd> Matrice<T> {
    #[inline]
    pub fn polygon_area(&self) -> Option<T> {
        if self.columns != 2 || self.rows < 2 {
            return None
        }

        let mut answer: T = num::zero();
        for n in range(0, self.rows()) {
            let n_row = n * 2;
            answer = if n == 0 { 
                answer + self.elems[0] * self.elems[3] 
                    - self.elems[2] * self.elems[1]
            } else if n + 1 == self.rows {
                answer + self.elems[n_row] * self.elems[1]
                    - self.elems[0] * self.elems[n_row + 1]
            } else {
                answer + self.elems[n_row] * self.elems[n_row + 3] -
                    self.elems[n_row + 2] * self.elems[n_row + 1]
            }
        }

        let one: T = num::one();

        if answer > num::zero() {
            Some(answer / (one + one))
        } else {
            Some(-answer / (one + one))
        }
    }
}

pub trait SquareRoot<T> {
    fn sqrt(&self) -> T;
}

impl SquareRoot<f32> for f32 {
    fn sqrt(&self) -> f32 {
        self.sqrt()
    }
}

impl SquareRoot<f64> for f64 {
    fn sqrt(&self) -> f64 {
        self.sqrt()
    }
}

impl<T: Num + SquareRoot<T>> Matrice<T> {
    #[inline]
    pub fn euclid_norm(&self) -> Option<T> {
        if (self.rows == 1) ^ (self.cols() == 1) {
            Some(self.elems.iter().map(|x| *x * *x).sum().sqrt())
        } else {
            None
        }
    }

    #[inline]
    pub fn cross_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        match ((self.rows, self.columns), (other.rows, other.columns)) {
            ((1, 3), (1, 3)) |
            ((3, 1), (3, 1)) => {
                Some(Matrice { 
                    rows: self.rows, columns: self.columns, elems:
                    vec!(self.elems[1] * other.elems[2] - self.elems[2] * other.elems[1],
                         self.elems[2] * other.elems[0] - self.elems[0] * other.elems[2],
                         self.elems[0] * other.elems[1] - self.elems[1] * other.elems[0])
                })
            },
            (_, _) => None
        }
    }
}

pub struct MatriceIterator<'a, T> {
    elems: &'a [T],
    jump: uint,
}

impl<'a, T> Iterator<&'a T> for MatriceIterator<'a, T> {
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
    #[inline]
    fn add(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs + *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Sub<T, T>> Sub<Matrice<T>, Matrice<T>> for Matrice<T> {
    #[inline]
    fn sub(&self, other: &Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs - *rhs).collect();


        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Num + Clone + fmt::Show> Mul<Matrice<T>, Matrice<T>> for Matrice<T> {
    #[inline]
    fn mul(&self, other: &Matrice<T>) -> Matrice<T> {
        if self.columns != other.rows {
            fail!(MismatchedAxes.to_string())
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

impl<T: Num + PartialOrd + Clone + fmt::Show> Div<Matrice<T>, Matrice<T>> for Matrice<T> {
    #[inline]
    fn div(&self, other: &Matrice<T>) -> Matrice<T> {
        let inverse = match other.inverse() {
            Some(x) => x,
            None => fail!("rhs does not have an inverse!".to_string())
        };

        *self * inverse
    }
}

impl<T: Num + Clone + fmt::Show> Neg<Matrice<T>> for Matrice<T> {
    #[inline]
    fn neg(&self) -> Matrice<T> {
        use std::num;
        let one: T = num::one();
        self.scalar(&-one, |a, b| *a * *b)
    }
}
