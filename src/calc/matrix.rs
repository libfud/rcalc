//! Matrices

use std::fmt;
use super::{BadArgType, CalcResult, LiteralType};

pub enum Dimension {
    Null,
    OneD(Vec<LiteralType>),
    TwoD(Vec<Vec<LiteralType>>),
    ThreeD(Vec<Vec<Vec<LiteralType>>>),
    FourD(Vec<Vec<Vec<Vec<LiteralType>>>>),
}

pub enum Axes {
    X, Y, Z, T,
    XY, XZ, XT, YZ, YT, ZT,
    XYZ, XYT, XZT, YZT,
    XYZT,
}

impl fmt::Show for Axes {
    fn fmt(&self, _: &mut fmt::formatter)

pub struct Matrice {
    x_len: Option<uint>,
    y_len: Option<uint>,
    z_len: Option<uint>,
    t_len: Option<uint>,
    elems: Dimension
}

impl Matrice {
    fn new(axes: Option<Axes>) -> Matrice {
        let (x, y, z, t) = if axes.is_some() {
            match axes.unwrap() {
                X    => (Some(0), None, None, None),
                Y    => (None, Some(0), None, None),
                Z    => (None, None, Some(0), None),
                T    => (None, None, None, Some(0)),
                XY   => (Some(0), Some(0), None, None),
                XZ   => (Some(0), None, Some(0), None),
                XT   => (Some(0), None, None, Some(0)),
                YZ   => (None, Some(0), Some(0), None),
                YT   => (None, Some(0), None, Some(0)),
                ZT   => (None, None, Some(0), Some(0)),
                XYZ  => (Some(0), Some(0), Some(0), None),
                XYT  => (Some(0), Some(0), None, Some(0)),
                XZT  => (Some(0), None, Some(0), Some(0)),
                YZT  => (None, Some(0), Some(0), Some(0)),
                XYZT => (Some(0), Some(0), Some(0), Some(0))
            }
        } else {
            (None, None, None, None)
        };

        let elems = if axes.is_some() {
            match axes.unwrap() {
                X | Y | Z | T => OneD(vec![]),
                XY | XZ | XT | YT | YZ | ZT => TwoD(vec![]),
                XYZ | XYT | XZT | YZT => ThreeD(vec![]),
                XYZT => FourD(vec![])
            }
        } else {
            Null
        };

        Matrice { x_len: x, y_len: y, z_len: z, t_len: t, elems: elems }
    }
    
    pub fn new_null() -> Matrice {
        Matrice::new(None)
    }

    pub fn new_1D(axis: Axes) -> CalcResult<Matrice> {
        match axis {
            X | Y | Z | T => Matrice::new(Some(axis)),
            _ => Err(BadArgType(format!("Cannot create a 1D matrix with 
}
