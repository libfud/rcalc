#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;

#[deriving(Show, Clone)]
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
            &TwoD((_, x), (_, y)) -> vec!(x, y),
            &ThreeD((_, x), (_, y), (_, z)) -> vec!(x, y, z),
            &FourD((_, x), (_, y), (_, z), (_, t)) => vec!(x, y, z, t),
        }
    }

    pub fn get_axes(&self) -> Vec<Axis> {
        match self {
            &Null => vec![],
            &OneD(x, _) => vec!(x),
            &TwoD((x, _), (y, _)) -> vec!(x, y),
            &ThreeD((x, _), (y, _), (z, _)) -> vec!(x, y, z),
            &FourD((x, _), (y, _), (z, _), (t, _)) => vec!(x, y, z, t),
        }
    }
}

impl fmt::Show for Dimensionality {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        let axes = self.get_axes();
        let dimens = self.get_dimens();
        for (a, b) in axes.iter().zip(dimens.iter()) {
            print!("{} : {} ", a, b);
        }
        Ok(())
    }
}

pub type Dim = Dimensionality;

pub type MatrixResult<T> = Result<T, MatrixErrors>;

#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Matrice<T> {
    dimensionality: Dimensionality,
    elems: Vec<T>,
}

impl<T: fmt::Show > fmt::Show for Matrice<T> {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!("{}", self.dimensionality);
        match self.dimensionality {
            Null => { },
            OneD(_) => println!("{}", self.elems),
            TwoD(x, y) => {
                for column in range(0, x) {
                    println!("{} ", self.elems.slice(column * y, column * y + y));
                }
            }
            _ => println!("I don't know yet.\n{}", self.elems),
        }
        Ok(())
    }
}            

trait FakeNum<T, U>: Add<T, Result<T, U>> + 
    Sub<T, Result<T, U>> + Mul<T, Result<T, U>> + 
    Div<T, Result<T, U>> + Rem<T, Result<T, U>> { }

impl<T: Clone + FakeNum<T, U>, U> Matrice<T> {
    pub fn new_null() -> Matrice<T> {
        Matrice { dimensionality: Null, elems: vec![] }
    }

    pub fn new_empty(axes: Vec<Axis>) -> Matrice<T> {
        match axes.len() {
            0 => Matrice { dimensionality: Null, elems: vec![] },
            1 => Matrice { dimensionality: OneD(axes.as_slice()[0], 

    pub fn scalar<U>(&self, scalar: &T, 
                     op: |&T, &T| -> Result<T, U>) -> MatrixResult<Matrice<T>> {
        let mut elems = Vec::new();
        for x in self.elems.iter() {
            let res = op(x, scalar);
            match res {
                Ok(x) => elems.push(x),
                Err(_) => return Err(BadMatrixOp)
            }
        }

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: elems })
    }
    
    pub fn extend_1d(&mut self, other: Vec<T>) -> MatrixResult<()> {
        self.elems.push_all(other.as_slice());
        self.dimensionality = OneD(self.elems.len());
        Ok(())
    }

        

    pub fn set_2d(&mut self, other: Vec<T>, other_dim: Dim) -> MatrixResult<()> {
        let (other_len, other_wid) = match other_dim {
            TwoD(x, y) => (x, y),
            _ => return Err(MismatchedAxes),
        };

        if other_len * other_wid != other.len() {
            return Err(BadDimensionality)
        }

        self.dimensionality = TwoD(other_len, other_wid);
        self.elems = other;

        Ok(())
    }

    pub fn extend_2d(&mut self, other: Vec<T>, other_dim: Dim) -> MatrixResult<()> {
        let (length, width) = match self.dimensionality {
            TwoD(x, y) => (x, y),
            x => fail!(format!("Expected a TwoD matrix, found {}!", x))
        };

        let (other_len, other_wid) = match other_dim {
            TwoD(x, y) => (x, y),
            _ => return Err(MismatchedAxes),
        };
        
        if other_wid != width || other_len * other_wid != other.len() {
            return Err(MismatchedAxes)
        }

        let new_len = length + other_len;
        let mut new_elems = Vec::new();
        for column in range(0, width) {
            new_elems.push_all(self.elems.slice(column * length, column * length + length));
            new_elems.push_all(other.slice(column * other_len, column * other_len + other_len));
        }

        self.dimensionality = TwoD(new_len, width);
        self.elems = new_elems;
        Ok(())
    }
}
 
impl<T: Add<T, Result<T, U>>, U> Add<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn add(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Sub<T, Result<T, U>>, U> Sub<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn sub(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Sub<T, Result<T, U>>, U> Sub<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn sub(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Mul<T, Result<T, U>>, U> Mul<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn mul(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Div<T, Result<T, U>>, U> Div<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn div(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}

impl<T: Rem<T, Result<T, U>>, U> Rem<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn rem(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), elems: new_elems })
    }
}
