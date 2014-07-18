use std::fmt;
use super::{MatrixResult, BadDimensionality, InvalidAxis, MismatchedAxes};

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
                let (x, len) = dimens[0].clone();
                Ok(OneD(x, len))
            },
            2 => {
                let (x, len) = dimens[0].clone();
                let (y, wid) = dimens[1].clone();
                if x == y {
                    return Err(BadDimensionality)
                }
                Ok(TwoD((x, len), (y, wid)))
            }
            3 => {
                let (x, len) = dimens[0].clone();
                let (y, wid) = dimens[1].clone();
                let (z, h) = dimens[2].clone();
                if x == y || x == z || y == z {
                    return Err(BadDimensionality)
                }
                Ok(ThreeD((x, len), (y, wid), (z, h)))
            }
            4 => {
                let (x, len) = dimens[0].clone();
                let (y, wid) = dimens[1].clone();
                let (z, h) = dimens[2].clone();
                let (t, s) = dimens[3].clone();
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
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let axes = self.get_axes();
        let dimens = self.get_lens();
        for (a, b) in axes.iter().zip(dimens.iter()) {
            try!(write!(fmt, "{} : {} ", a, b));
        }
        Ok(())
    }
}

pub type Dim = Dimensionality;


#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Tensor<T> {
    dimensionality: Dimensionality,
    elems: Vec<T>,
}

impl<T: fmt::Show > fmt::Show for Tensor<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(fmt, "{}", self.dimensionality));
        let lens = self.dimensionality.get_lens();
        match self.dimensionality {
            Null => { },
            OneD(_, _) => try!(writeln!(fmt, "{}", self.elems)),
            TwoD(_, _) => {
                let (a, b) = (lens[0], lens[1]);
                for column in range(0, a) {
                    try!(write!(fmt, "{} ", self.elems.slice(column * b, column * b + b)));
                }
            }
            _ => try!(writeln!(fmt, "I don't know yet.\n{}", self.elems)),
        }
        Ok(())
    }
}            

impl<T: Clone + Num, U> Tensor<T> {
    pub fn new_null() -> Tensor<T> {
        Tensor { dimensionality: Null, elems: vec![] }
    }

    pub fn new_empty(axes: Vec<Axis>) -> MatrixResult<Tensor<T>> {
        if axes.len() > 4 {
            return Err(BadDimensionality)
        }
        
        if axes.len() > 0 {
            let mut a = &axes[0];
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

    pub fn scalar(&self, scalar: &T, op: |&T, &T| -> T) -> Tensor<T> {
        let elems: Vec<T> = self.elems.iter().map(|a| op(a, scalar)).collect();
        Tensor { dimensionality: self.dimensionality.clone(), elems: elems }
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
            1 => axes[0].clone(),
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
            2 => (scalars[0].clone(), scalars[1].clone()),
            _ => return Err(BadDimensionality)
        };

        let other_scalars = other_dim.get_lens();
        let (other_len, other_wid) = match other_scalars.len() {
            2 => (other_scalars[0].clone(), other_scalars[1].clone()),
            _ => return Err(BadDimensionality),
        };

        let axes = self.dimensionality.get_axes();
        let (a, b) = match axes.len() {
            2 => (axes[0], axes[1]),
            _ => return Err(BadDimensionality),
        };


        let other_axes = other_dim.get_axes();
        let (x, y) = match other_axes.len() {
            2 => (other_axes[0], other_axes[1]),
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
 
impl<T: Add<T, T>> Add<Tensor<T>, Tensor<T>> for Tensor<T> {
    fn add(&self, other: &Tensor<T>) -> Tensor<T> {
        if self.dimensionality != other.dimensionality {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs + *rhs).collect();

        Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems }
    }
}

impl<T: Sub<T, T>> Sub<Tensor<T>, Tensor<T>> for Tensor<T> {
    fn sub(&self, other: &Tensor<T>) -> Tensor<T> {
        if self.dimensionality != other.dimensionality {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs - *rhs).collect();

        Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems }
    }
}

impl<T: Mul<T, T>> Mul<Tensor<T>, Tensor<T>> for Tensor<T> {
    fn mul(&self, other: &Tensor<T>) -> Tensor<T> {
        if self.dimensionality != other.dimensionality {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs * *rhs).collect();

        Tensor{ dimensionality: self.dimensionality.clone(), elems: new_elems }
    }
}

impl<T: Div<T, T>> Div<Tensor<T>, Tensor<T>> for Tensor<T> {
    fn div(&self, other: &Tensor<T>) -> Tensor<T> {
        if self.dimensionality != other.dimensionality {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs / *rhs).collect();

        Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems }
    }
}

impl<T: Rem<T, T>> Rem<Tensor<T>, Tensor<T>> for Tensor<T> {
    fn rem(&self, other: &Tensor<T>) -> Tensor<T> {
        if self.dimensionality != other.dimensionality {
            fail!(MismatchedAxes.to_string())
        }

        let new_elems: Vec<T> = self.elems.iter().zip(other.elems.iter()).map(
            |(lhs, rhs)| *lhs % *rhs).collect();

        Tensor { dimensionality: self.dimensionality.clone(), elems: new_elems }
    }
}
