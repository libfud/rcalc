#![crate_name = "matrix"]
#![crate_type = "lib"]
#![feature(core,collections)]

extern crate num;

use num::traits::{Num, Zero, One};
use std::fmt;
use std::cmp;
use std::mem;
use std::ops::{Add,Sub,Neg,Mul,Div};
use std::iter::repeat;

#[cfg(use_fancy)]
use fancy::{UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT, MIDDLE_LEFT, MIDDLE_RIGHT};
#[cfg(not(use_fancy))]
use not_fancy::{UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT, MIDDLE_LEFT, MIDDLE_RIGHT};

#[cfg(test)]
mod tests;

#[cfg(use_fancy)]
mod fancy {
    pub static UPPER_LEFT: &'static str = "⎡";
    pub static UPPER_RIGHT: &'static str = "⎤";
    pub static LOWER_LEFT: &'static str = "⎣";
    pub static LOWER_RIGHT: &'static str = "⎦";
    pub static MIDDLE_LEFT: &'static str = "⎢";
    pub static MIDDLE_RIGHT: &'static str = "⎥";
}

#[cfg(not(use_fancy))]
mod not_fancy {
    pub static UPPER_LEFT: &'static str = "[";
    pub static UPPER_RIGHT: &'static str = "]";
    pub static LOWER_LEFT: &'static str = "[";
    pub static LOWER_RIGHT: &'static str = "]";
    pub static MIDDLE_LEFT:  &'static str = "|";
    pub static MIDDLE_RIGHT: &'static str = "|";
}


#[derive(Clone, PartialEq)]
pub enum MatrixErrors {
    InvalidAxis,
    MismatchedAxes, 
    BadDimensionality,
    BadMatrixOp
}

pub type MatrixResult<T> = Result<T, MatrixErrors>;

impl fmt::Display for MatrixErrors {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}",  match *self {
            MatrixErrors::InvalidAxis => "Invalid axis",
            MatrixErrors::MismatchedAxes => "Mismatched axes",
            MatrixErrors::BadDimensionality => "Bad dimensionality",
            MatrixErrors::BadMatrixOp => "Bad matrix operation",
        }));
        Ok(())
    }
}

/* Matrices and their implementation */

#[derive(Clone, PartialOrd, PartialEq, Eq, Ord, Debug)]
pub struct Matrice<T> {
    columns: usize,
    rows: usize,
    elems: Vec<T>
}

impl<T: fmt::Display > fmt::Display for Matrice<T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut columns: Vec<Vec<String>> = Vec::new();
        let mut col_widths: Vec<usize> = Vec::new();

        for column in 0..self.columns {
            let mut col = Vec::with_capacity(self.rows);
            for row in 0.. self.rows {
                col.push(self.elems[column + row * self.columns].to_string());
            }
            col_widths.push(col.iter().fold(0, |a, b| cmp::max(a, b.len())));
            columns.push(col);
        }

        for row in 0..self.rows {
            try!(write!(fmt, "{}", if row == 0 {
                UPPER_LEFT
            } else if row == self.rows - 1 {
                LOWER_LEFT
            } else {
                MIDDLE_LEFT
            }));
            for col in 0..self.columns {
                let len = columns[col][row].len(); // space and comma
                try!(write!(fmt, "{a}{b}{c}", b = columns[col][row],
                            a = repeat(" ").take(col_widths[col] + 1 - len).collect::<String>(),
                            c = if col != self.columns - 1 { "," } else { "" }));
            }
            try!(writeln!(fmt, "{}", if row == 0 {
                UPPER_RIGHT
            } else if row == self.rows - 1 {
                LOWER_RIGHT
            } else {
                MIDDLE_RIGHT
            }));
        }                

        Ok(())
    }
}

impl<T: Clone> Matrice<T> { 
    #[inline]
    pub fn new() -> Matrice<T> {
        Matrice { columns: 0, rows: 0, elems: vec![] }
    }

    #[inline]
    pub fn scalar<F: Fn(T, T) -> T>(&self, rhs: &T, op: F) -> Matrice<T> {
        let new_elems = self.elems.iter().map(|lhs| op(lhs.clone(), rhs.clone())).collect();
        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }

    #[inline]
    pub fn rows(&self) -> usize {
        self.rows
    } 

    #[inline]
    pub fn cols(&self) -> usize {
        self.columns
    }

    #[inline]
    pub fn real_len(&self) -> usize {
        self.elems.len()
    }

    #[inline]
    pub fn get_row<'a>(&'a self, row: usize) -> MatriceIterator<'a, T> {
        MatriceIterator { 
//            elems: self.elems.slice(row * self.columns, (row + 1) * self.columns),
            elems: &self.elems[row * self.columns .. (row + 1) * self.columns],
            jump: 1
        }
    }

    #[inline]
    pub fn get_col<'a>(&'a self, col: usize) -> MatriceIterator<'a, T> {
        MatriceIterator {
//            elems: self.elems.slice_from(col),
            elems: &self.elems[col ..],
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
//            elems: self.elems.slice(self.columns - 1, (self.rows - 1) * self.columns + 1),
            elems: &self.elems[self.columns - 1 .. (self.rows - 1) * self.columns + 1],
            jump: self.columns - 1
        }
    }

    #[inline]
    pub fn swap_rows(&mut self, row_a: usize, row_b: usize) -> Result<(),MatrixErrors> {
        if row_a == row_b {
            return Ok(())
        } else if row_a > self.rows || row_b > self.rows {
            return Err(MatrixErrors::BadDimensionality)
        }

        let (small, big) = (cmp::min(row_a, row_b), cmp::max(row_a, row_b));
        let (low, high ) = self.elems.split_at_mut(big * self.columns);
//        let low = low.slice_mut(small * self.columns, (small + 1) * self.columns);
        let low = &mut low[small * self.columns .. (small + 1) * self.columns];
//        let high = high.slice_to_mut(self.columns);
        let high = &mut high[.. self.columns];

        for (low_elem, high_elem) in low.iter_mut().zip(high.iter_mut()) {
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
    pub fn from_vec(other: Vec<T>, width: usize, height: usize) -> MatrixResult<Matrice<T>> {
        if width * height != other.len() {
            Err(MatrixErrors::BadDimensionality)
        } else {
            Ok(Matrice { columns: width, rows: height, elems: other })
        }
    }

    #[inline]
    pub fn get_elem(&self, row: usize, column: usize) -> Option<T> {
        if row * self.columns + column > self.elems.len() {
            None
        } else {
            Some(self.elems[row * self.columns + column].clone())
        }
    }

    #[inline]
    pub fn append_row(&mut self, other: &mut Vec<T>) -> MatrixResult<()> {
        if other.len() != self.columns {
            Err(MatrixErrors::BadDimensionality)
        } else {
            self.rows = self.rows + 1;
/*
            let mut new_elems = self.elems.clone();
            new_elems.append(other);
*/
            self.elems.append(other);
            Ok(())
        }
    }

    #[inline]
    pub fn append_col(&mut self, other: Vec<T>) -> MatrixResult<()> {
        if other.len() != self.rows {
            Err(MatrixErrors::BadDimensionality)
        } else {
            let mut new_elems = Vec::with_capacity(self.elems.len() + other.len());
            for n in 0..self.rows {
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
    pub fn submatrix(&self, ofsx: usize, ofsy: usize, 
                     rows: usize, cols: usize) -> Option<Matrice<T>> {
        if ofsx + cols > self.columns || ofsy + rows > self.rows {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(rows * cols);
        for n in ofsy..(rows + ofsy) {
            new_elems.extend(self.get_row(n).skip(ofsx).take(cols).map(|x: &T| x.clone()));
        }

        Some(Matrice { columns: cols, rows: rows, elems: new_elems })
    }
    
    #[inline]
    pub fn concat_cols(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.rows != other.rows {
            return None
        }

        let mut new_elems = Vec::<T>::with_capacity(self.rows * 2 * (self.columns + other.columns));
        for n in 0.. self.rows {
            new_elems.extend(self.get_row(n).take(self.columns).map(|x: &T| x.clone()));
            new_elems.extend(other.get_row(n).take(other.columns).map(|x: &T| x.clone()));
        }
        Some(Matrice { columns: self.columns + other.columns, rows: self.rows,
                       elems: new_elems })
    }

    #[inline]
    pub fn concat_rows(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.columns != other.columns {
            return None
        }

        let mut new_elems = Vec::with_capacity(self.cols() * 2 * (self.rows + other.rows));
        new_elems.push_all(self.elems.as_slice());
        new_elems.push_all(other.elems.as_slice());
        
        Some(Matrice { columns: self.columns, rows: self.rows + other.rows,
                       elems: new_elems })
    }

    #[inline]
    pub fn set_row(&mut self, old_row: usize, new_row: Vec<T>) -> MatrixResult<()> {
        if old_row > self.rows || new_row.len() > self.columns {
            return Err(MatrixErrors::BadDimensionality);
        }

        for (elt, new) in ((old_row * self.columns)..
                                ((old_row + 1) * self.columns)).zip(new_row.iter()) {
            *self.elems.get_mut(elt).unwrap() = new.clone()
        }

        Ok(())
    }

    #[inline]
    pub fn set_col(&mut self, old_col: usize, new_col: Vec<T>) -> MatrixResult<()> {
        if old_col > self.columns || new_col.len() > self.rows {
            return Err(MatrixErrors::BadDimensionality)
        }

        for (elt, new) in (0..self.rows).zip(new_col.iter()) {
            *self.elems.get_mut(elt * self.rows + old_col).unwrap() = new.clone()
        }
        
        Ok(())
    }

    #[inline]
    pub fn decross(&self, omit_row: usize, omit_col: usize) -> Matrice<T> {
        let mut new_elems = Vec::new();
        for row in 0..self.rows {
            if row + 1 == omit_row {
                continue
            }
            for col in (0.. self.columns) {
                if col + 1 == omit_col {
                    continue
                } 
                new_elems.push(self.elems[row * self.columns + col].clone());
            }
        }

        Matrice { 
            rows: if omit_row == 0 || omit_row > self.rows {
                self.rows
            } else {
                self.rows - 1
            },
            columns: if omit_col == 0 || omit_col > self.columns {
                self.columns
            } else {
                self.columns - 1
            },
            elems: new_elems
        }
    }
}

impl<T: Clone + Zero + One + PartialEq> Matrice<T> {
    /// Return an identity matrix of rows & columns n.
    #[inline]
    pub fn ident(n: usize) -> Matrice<T> {
        let mut elems: Vec<T> = repeat(num::zero()).take(n * n).collect::<Vec<T>>();
//        let mut elems: Vec<T> = Vec::from_fn(n * n, |_| num::zero());
        for i in (0.. n) {
            *elems.get_mut(i * n + i).unwrap() = num::one();
        }
        
        Matrice { rows: n, columns: n, elems: elems }
    }

    #[inline]
    pub fn get_pivot<'a, It: Iterator<Item=&'a T>>(iterator: It) -> Option<(&'a T, usize)> {
        let zero: T = num::zero();
        iterator.enumerate().find(|&(_, x)| *x != zero)
            .map(|(idx, x)| (x, idx))
    }

    /// Return a vector of pivots and the column at which they occur.
    #[inline]
    pub fn row_pivots<'a>(&'a self) -> Vec<Option<(&'a T, usize)>> {
        (0..self.rows).map(|row| Matrice::get_pivot(self.get_row(row))).collect()
    }

    #[inline]
    pub fn col_pivots<'a>(&'a self) -> Vec<Option<(&'a T, usize)>> {
        (0..self.columns).map(|col| Matrice::get_pivot(self.get_col(col))).collect()
    }

    #[inline]
    pub fn is_ident(&self) -> bool {
        if self.rows != self.columns {
            false
        } else {
            let one: T = num::one();
            (0.. self.rows()).all(|i| {
                self.get_row(i).enumerate().all(|(j, x): (usize, &T)| if i == j {
                    *x == one 
                } else { 
                    x.is_zero()
                })
            })
        }
    }
}

impl<'a, T: PartialOrd + Clone + fmt::Display + Zero + One + Sub<Output=T> + 
    Div<Output=T> + Neg<Output=T> + Add<Output=T>> Matrice<T> {
    #[inline]
    pub fn trace(&self) -> Option<T> {
        if self.rows != self.columns {
            None
        } else {
            let zero: T = num::zero();
            Some(self.left_diag().fold(zero, |a: T, b: &T| a + b.clone()))
        }
    }
    
    #[inline]
    pub fn hadamard_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.rows != other.rows || self.columns != other.columns {
            None
        } else {
            Some(Matrice { rows: self.rows, columns: self.columns,
                           elems: self.elems.iter().zip(other.elems.iter()).map(|(x, y)| {
                               x.clone() * y.clone()}).collect() })
        }
    }

    #[inline]
    pub fn kronecker_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        if self.columns != other.rows {
            return None
        }

        let mut new_elems: Vec<T> = Vec::with_capacity(self.columns * other.rows);
        for row in (0.. self.rows) {
            for col in (0.. other.columns) {
//                new_elems.push(self.get_row(row).zip(other.get_col(col))
//                               .map(|(lhs, rhs): (&'a T, &'a T)| *lhs * *rhs).sum());
                let mut answer = num::zero();

                for elemx in self.get_row(row) {
                    for elemy in self.get_col(col) {
                        answer = answer + elemx.clone() * elemy.clone();
                    }
                }
                new_elems.push(answer);
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
            return Some((self.elems[0].clone() * self.elems[3].clone()) - 
                        (self.elems[1].clone() * self.elems[2].clone()))
        }

        if self.is_ident() {
            return Some(num::one())
        }

        let mut upper = self.clone();

        /* We're going to change the tail rows so that we get into row echelon form. */
        let mut swaps = 0usize;
        for row in (0.. self.rows) {
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
                    Err(m) => panic!(m.to_string())
                }
            }

            /* We're working to zero out those columns appropiately, so for a 3x3, 
             * so the divisors will be along the main diagonal */
            let divisor = upper.elems[row * self.columns + row].clone();

            for next_row in (row + 1.. self.rows) {
                let lower_elt = upper.elems[next_row * self.columns + row].clone() / divisor.clone();

                let new_row: Vec<T> = upper.get_row(next_row).zip(upper.get_row(row))
                    .map(|(x, y)| x.clone() - (y.clone() * lower_elt.clone())).collect();

                match upper.set_row(next_row, new_row) {
                    Ok(_) => { },
                    Err(_) => return None
                }
            }
        }

        let one: T = num::one();
        let prod = upper.left_diag().fold(one, |a: T, b: &T| a * b.clone());

        if swaps % 2 == 0 {
            Some(prod)
        } else {
            Some(-prod)
        }
    }

    #[inline]
    pub fn transpose(&self) -> Matrice<T> {
        let mut new_elems = Vec::with_capacity(self.elems.len());
        for column in (0.. self.columns) {
            new_elems.extend(self.get_col(column).map(|x: &T| x.clone()));
        }

        Matrice { rows: self.columns, columns: self.rows, elems: new_elems }
    }

    #[inline]
    pub fn translate_by(&self, other: &[T]) -> Option<Matrice<T>> {
        if other.len() != self.rows { 
            return None
        }

        let mut new_elems = Vec::with_capacity(self.elems.len());
        for row in (0.. self.rows) {
            new_elems.extend(self.get_row(row).map(|x: &T| x.clone() + other[row].clone()))
        }

        Some(Matrice { rows: self.rows, columns: self.columns, elems: new_elems })
    }

    /// Returns a matrix of minors.
    #[inline]
    pub fn minors(&self) -> Option<Matrice<T>> {
        if self.rows != self.columns || self.rows == 0 {
            return None
        }

        let mut minors: Vec<T> = Vec::with_capacity(self.rows * self.rows);
        for row in (0.. self.rows) {
            for column in (0.. self.cols()) {
                let submatrix = self.decross(row + 1, column + 1);
                minors.push(submatrix.determinant().unwrap_or(return None));
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
            return Some( Matrice { rows: 1, columns: 1, elems: vec!(one / self.elems[0].clone())})
        }

        let mut minors = match self.minors() {
            Some(x) => x,
            None => return None
        };

/*        let determinant: T = range(0, self.columns).map(|n| {
            if n % 2 == 0 {
                self.elems[n] * minors.elems[n]
            } else {
                -(self.elems[n] * minors.elems[n])
            }
        }).sum();
*/

        let mut determinant: T = num::zero();
        for n in (0.. self.columns) {
            if n % 2 == 0 {
                determinant = determinant.clone() + self.elems[n].clone() * minors.elems[n].clone()
            } else {
                determinant = determinant.clone() - self.elems[n].clone() * minors.elems[n].clone()
            }
        }

        if determinant.is_zero() {
            return None
        }

        for row in (0.. minors.rows) {
            for x in 0.. minors.columns {
                let i = row * self.cols() + x;
                if (row % 2 == 0)^(x % 2 == 0) {
                    *minors.elems.get_mut(i).unwrap() = minors.elems[i].clone() / 
                        -determinant.clone()
                } else {
                    *minors.elems.get_mut(i).unwrap() = minors.elems[i].clone() / 
                        determinant.clone()
                }
            }
        }
        
        Some(minors.transpose())
    }
}

impl<T: Clone + PartialOrd + Zero + Add + Sub<Output=T> + Mul + One + Div<Output=T> + Neg<Output=T>>
    Matrice<T> {
    #[inline]
    pub fn polygon_area(&self) -> Option<T> {
        if self.columns != 2 || self.rows < 2 {
            return None
        }

        let mut answer: T = num::zero();
        for n in 0..self.rows() {
            let n_row = n * 2;
            answer = if n == 0 { 
                answer + self.elems[0].clone() * self.elems[3].clone() 
                    - self.elems[2].clone() * self.elems[1].clone()
            } else if n + 1 == self.rows {
                answer + self.elems[n_row].clone() * self.elems[1].clone()
                    - self.elems[0].clone() * self.elems[n_row + 1].clone()
            } else {
                answer + self.elems[n_row].clone() * self.elems[n_row + 3].clone() -
                    self.elems[n_row + 2].clone() * self.elems[n_row + 1].clone()
            }
        }

        let one: T = num::one();

        if answer > num::zero() {
            Some(answer / (one + num::one()))
        } else {
            Some(-answer / (one + num::one()))
        }
    }
}

pub trait SquareRoot<T> {
    fn sqrt(&self) -> T;
}

impl SquareRoot<f32> for f32 {
    fn sqrt(&self) -> f32 {
        std::num::Float::sqrt(*self)
//        self.sqrt()
    }
}

impl SquareRoot<f64> for f64 {
    fn sqrt(&self) -> f64 {
        std::num::Float::sqrt(*self)
//        self.sqrt()
    }
}

impl<T: SquareRoot<T> + Mul<Output=T> + Sub<Output=T> + Add<Output=T> + Zero + Clone> Matrice<T> {
    #[inline]
    pub fn euclid_norm(&self) -> Option<T> {
        if (self.rows == 1) ^ (self.cols() == 1) {
//            Some(self.elems.iter().map(|x| *x * *x).sum().sqrt())
            let mut answer = num::zero();
            for elem in self.elems.iter() {
                answer = answer + elem.clone() * elem.clone();
            }
            Some(answer)
        } else {
            None
        }
    }

    #[inline]
    pub fn cross_prod(&self, other: &Matrice<T>) -> Option<Matrice<T>> {
        match ((self.rows, self.columns), (other.rows, other.columns)) {
            ((1, 3), (1, 3)) | ((3, 1), (3, 1)) => {
                Some(Matrice { rows: self.rows, columns: self.columns, elems:
                               vec!(self.elems[1].clone() * other.elems[2].clone()
                                    - self.elems[2].clone() * other.elems[1].clone(),
                                    self.elems[2].clone() * other.elems[0].clone()
                                    - self.elems[0].clone() * other.elems[2].clone(),
                                    self.elems[0].clone() * other.elems[1].clone()
                                    - self.elems[1].clone() * other.elems[0].clone())
                })
            },
            (_, _) => None
        }
    }
}

pub struct MatriceIterator<'a, T: 'a> {
    elems: &'a [T],
    jump: usize,
}

impl<'a, T> Iterator for MatriceIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let val = self.elems.get(0);
//        self.elems = self.elems.slice_from(cmp::min(self.jump, self.elems.len()));
        self.elems = &self.elems[cmp::min(self.jump, self.elems.len()) ..];
        val
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.elems.len() / self.jump;
        (size, Some(size))
    }
}

//impl<T: Add> Add for Matrice<T> {
impl<T: Add<Output=T> + Clone> Add<Matrice<T>> for Matrice<T> {
    #[inline]

    type Output = Matrice<T>;

    fn add(self, other: Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            panic!(MatrixErrors::MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| lhs.clone() + rhs.clone()).collect();

        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Sub<Output=T> + Clone> Sub for Matrice<T> {
//impl<T: Sub<T, T>> Sub<Matrice<T>, Matrice<T>> for Matrice<T> {
    #[inline]

    type Output = Matrice<T>;

    fn sub(self, other: Matrice<T>) -> Matrice<T> {
        if (self.columns, self.rows) != (other.columns, other.rows) {
            panic!(MatrixErrors::MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| lhs.clone() - rhs.clone()).collect();

        Matrice { columns: self.columns, rows: self.rows, elems: new_elems }
    }
}

impl<T: Num + Neg<Output=T> + Mul<Output=T> + Clone> Neg for Matrice<T> {
//impl<T: Num + Clone + fmt::Display> Neg<Matrice<T>> for Matrice<T> {
    #[inline]
    type Output = Matrice<T>;

    fn neg(self) -> Matrice<T> {
        let one: T = num::one();
        self.scalar(&-one, |a: T, b: T| a * b)
    }
}

