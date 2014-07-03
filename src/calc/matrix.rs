//! Matrices

use std::fmt;
use super::{BadArgType, CalcResult, LiteralType};

pub enum Dimensionality {
    Null,
    OneD(uint),
    TwoD(uint, uint),
    ThreeD(uint, uint, uint),
    FourD(uint, uint, uint, uint),
}

/*
#[deriving(Show)]
pub enum Axes {
    X, Y, Z, T,
    XY, XZ, XT, YZ, YT, ZT,
    XYZ, XYT, XZT, YZT,
    XYZT,
}
*/

bitflags!(
    #[deriving(Show)]
    flags Axes: u8 {
        static Empty = 0u8,

        static X = 1u8, static Y = 2u8, static Z = 4u8, static T = 8u8,

        static XY = X.bits | Y.bits, static XZ = X.bits | Z.bits,
        static XT = X.bits | T.bits, static YZ = Y.bits | Z.bits,
        static YT = Y.bits | T.bits, static ZT = Z.bits | T.bits,

        static XYZ = XY.bits | Z.bits, static XYT = XY.bits | T.bits,
        static XZT = XZ.bits | T.bits, static YZT = YZ.bits | T.bits,

        static XYZT = XY.bits | ZT.bits
    }
)
        

pub struct Matrice {
    dimensionality: Dimensionality,
    axes: Axes,
    elems: Vec<LiteralType>,
}

impl Matrice {
    fn new(axes: Axes) -> Matrice {
        let dim = match axes {
            X | Y | Z | T => OneD(0),
            XY | XZ | XT | YZ | YT | ZT => TwoD(0, 0),
            XYZ | XYT | XZT | YZT => ThreeD(0, 0, 0),
            XYZT => FourD(0, 0, 0, 0),
            _ => Null
        };
        
        Matrice { dimensionality: dim, axes: axes, elems: vec![] }
    }
    
    pub fn new_null() -> Matrice {
        Matrice::new(Empty)
    }

    pub fn new_1D(axis: Axes) -> CalcResult<Matrice> {
        match axis {
            X | Y | Z | T => Ok(Matrice::new(axis)),
            _ => Err(BadArgType(format!("Cannot create a 1D matrix with {} dimensions!",
                                        axis)))
        }
    }

    pub fn new_2d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XY | XZ | XT | YZ | YT | ZT => Ok(Matrice::new(axes)),
            _ => Err(BadArgType(format!("Cannot create a 2D matrix with {} dimensions!",
                                        axes)))
        }
    }

    pub fn new_3d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XYZ | XYT | XZT | YZT => Ok(Matrice::new(axes)),
            _ => Err(BadArgType(format!("Cannot create a 3D matrix with {} dimensions!",
                                        axes)))
        }
    }

    pub fn new_4d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XYZT => Ok(Matrice::new(axes)),
            _ => Err(BadArgType(format!("Cannot create a 4D matrix with {} dimensions",
                                        axes)))
        }
    }
}
