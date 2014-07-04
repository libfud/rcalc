#![crate_id = "matrix"]
#![crate_type = "lib"]

use std::fmt;

#[deriving(Show, Clone)]
pub enum MatrixErrors {
    InvalidAxis,
    MismatchedAxes, 
    BadDimensionality,
    NullAxisExtension
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
    dimensionality: Dimensionality,
    axes: Option<Axes>,
    elems: Vec<T>,
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

impl<T: Clone> Matrice<T> {
    pub fn new<T>(axes: Option<Axes>) -> Matrice<T> {
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

        Matrice { dimensionality: dim, axes: axes, elems: vec![] }
    }
    
    pub fn extend_1d<T: Clone>(&mut self, axis: Axes, other: Vec<T>) -> MatrixResult<()> {
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

    pub fn set_2d<T: Clone>(&mut self, axes: Axes, other: Vec<T>, 
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

    pub fn extend_2d<T: Clone>(&mut self, axes: Axes, other: Vec<T>, 
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
