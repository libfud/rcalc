#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;

#[deriving(Show, Clone)]
pub enum MatrixErrors {
    InvalidAxis,
    MismatchedAxes, 
    BadDimensionality,
    NullAxisExtension,
    BadMatrixOp
}

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Dimensionality {
    Null,
    OneD(uint),
    TwoD(uint, uint),
    ThreeD(uint, uint, uint),
    FourD(uint, uint, uint, uint),
}

pub type Dim = Dimensionality;

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Axes {
    X, Y, Z, T,
    XY, XZ, XT, YZ, YT, ZT,
    XYZ, XYT, XZT, YZT,
    XYZT,
}

pub type MatrixResult<T> = Result<T, MatrixErrors>;

impl Axes {
    pub fn from_str(s: &String) -> MatrixResult<Option<Axes>> {
        let res = match s.as_slice() {
            "X" => Some(X), "Y" => Some(Y), "Z" => Some(Z), "T" => Some(T),

            "XY" => Some(XY), "XZ" => Some(XZ), "XT" => Some(XT),
            "YZ" => Some(YZ), "YT" => Some(YT), "ZT" => Some(ZT),

            "XYZ" => Some(XYZ), "XYT" => Some(XYT), "XZT" => Some(XZT),
            "YZT" =>  Some(YZT),

            "XYZT" => Some(XYZT),

            "null" => None,

            _ => return Err(InvalidAxis)
        };
        Ok(res)
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Matrice<T> {
    pub dimensionality: Dimensionality,
    pub axes: Option<Axes>,
    pub elems: Vec<T>,
}

impl<T: fmt::Show > fmt::Show for Matrice<T> {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!("{} {} axes defined", self.dimensionality, match self.axes {
            Some(x) => x.to_str(),
            None => "no".to_str(),
        });
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

impl<T: Clone + Add<T, Result<T, U>> + Sub<T, Result<T, U>> + 
    Mul<T, Result<T, U>> + Div<T, Result<T, U>> + Rem<T, Result<T, U>>, U> Matrice<T> {
    pub fn new(axes: Option<Axes>) -> Matrice<T> {
        let dim = if axes.is_some() {
            match axes.unwrap() {
                X | Y | Z | T => OneD(0),
                XY | XZ | XT | YZ | YT | ZT => TwoD(0, 0),
                XYZ | XYT | XZT | YZT => ThreeD(0, 0, 0),
                XYZT => FourD(0, 0, 0, 0),
            }
        } else {
            Null
        };

        let elems: Vec<T> = Vec::new();
        Matrice { dimensionality: dim, axes: axes, elems: elems }
    }

    pub fn scalar<U>(&self, scalar: &T, op: |&T, &T| -> Result<T, U>) -> MatrixResult<Matrice<T>> {
        let mut elems = Vec::new();
        for x in self.elems.iter() {
            let res = op(x, scalar);
            match res {
                Ok(x) => elems.push(x),
                Err(_) => return Err(BadMatrixOp)
            }
        }

        Ok(Matrice { dimensionality: self.dimensionality.clone(),
                  axes: self.axes.clone(),
                  elems: elems
        })
    }
    
    pub fn extend_1d(&mut self, axis: Axes, other: Vec<T>) -> MatrixResult<()> {
        if self.axes.is_none() {
            return Err(NullAxisExtension)
        }

        match axis {
            X | Y | Z | T => { },
            _ => return Err(BadDimensionality)
        }

        if self.axes.unwrap() != axis {
            return Err(MismatchedAxes)
        }

        self.elems.push_all(other.as_slice());
        self.dimensionality = OneD(self.elems.len());
        Ok(())
    }

    pub fn set_2d(&mut self, axes: Axes, other: Vec<T>, 
                            other_dim: Dim) -> MatrixResult<()> {
        if self.axes.is_none() {
            return Err(NullAxisExtension)
        }

        match axes {
            XY | XZ | XT | YZ | YT | ZT => { },
            _ => return Err(BadDimensionality)
        }

        if self.axes.unwrap() != axes {
            return Err(MismatchedAxes)
        }

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

    pub fn extend_2d(&mut self, axes: Axes, other: Vec<T>, 
                               other_dim: Dim) -> MatrixResult<()> {
        if self.axes.is_none() {
            return Err(NullAxisExtension)
        }

        match axes {
            XY | XZ | XT | YZ | YT | ZT => { },
            _ => {
                return Err(BadDimensionality)
            }
        }

        if self.axes.unwrap() != axes {
            return Err(MismatchedAxes)
        }

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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), 
                     axes: self.axes.clone(),
                     elems: new_elems
        })
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), 
                     axes: self.axes.clone(),
                     elems: new_elems
        })
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), 
                     axes: self.axes.clone(),
                     elems: new_elems
        })
    }
}

impl<T: Div<T, Result<T, U>>, U> Div<Matrice<T>, MatrixResult<Matrice<T>>> for Matrice<T> {
    fn div(&self, other: &Matrice<T>) -> MatrixResult<Matrice<T>> {
        if self.dimensionality != other.dimensionality {
            return Err(MismatchedAxes)
        }
        let mut new_elems = Vec::new();
        for (a, b) in self.elems.iter().zip(other.elems.iter()) {
            new_elems.push( match *a / *b {
                Ok(x) => x,
                Err(_) => return Err(BadMatrixOp)
            });
        }

        Ok(Matrice { dimensionality: self.dimensionality.clone(), 
                     axes: self.axes.clone(),
                     elems: new_elems
        })
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

        Ok(Matrice { dimensionality: self.dimensionality.clone(), 
                     axes: self.axes.clone(),
                     elems: new_elems
        })
    }
}
