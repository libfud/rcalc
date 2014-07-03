//! Matrices

use std::fmt;
use super::{ArgType, Atom, BigRational, CalcResult, Environment};
use super::{BadArgType, BadNumberOfArgs, BadDimensionality};
use super::literal::{LiteralType, BigNum, List, Matrix, Symbol, Void};

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Dimensionality {
    Null,
    OneD(uint),
    TwoD(uint, uint),
    ThreeD(uint, uint, uint),
    FourD(uint, uint, uint, uint),
}

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Axes {
    X, Y, Z, T,
    XY, XZ, XT, YZ, YT, ZT,
    XYZ, XYT, XZT, YZT,
    XYZT,
}

impl Axes {
    pub fn from_str(s: &String) -> CalcResult<Option<Axes>> {
        let res = match s.as_slice() {
            "X" => Some(X), "Y" => Some(Y), "Z" => Some(Z), "T" => Some(T),

            "XY" => Some(XY), "XZ" => Some(XZ), "XT" => Some(XT),
            "YZ" => Some(YZ), "YT" => Some(YT), "ZT" => Some(ZT),

            "XYZ" => Some(XYZ), "XYT" => Some(XYT), "XZT" => Some(XZT),
            "YZT" =>  Some(YZT),

            "XYZT" => Some(XYZT),

            "null" => None,

            _ => return Err(BadArgType("Invalid argument for axes".to_str()))
        };
        Ok(res)
    }
}


#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub struct Matrice {
    dimensionality: Dimensionality,
    axes: Option<Axes>,
    elems: Vec<BigRational>,
}

impl Matrice {
    fn new(axes: Option<Axes>) -> Matrice {
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
    
    pub fn new_null() -> Matrice {
        Matrice::new(None)
    }

    pub fn new_1d(axis: Axes) -> CalcResult<Matrice> {
        match axis {
            X | Y | Z | T => Ok(Matrice::new(Some(axis))),
            _ => Err(BadArgType(format!("Cannot create a 1D matrix with {} dimensions!",
                                        axis)))
        }
    }

    pub fn new_2d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XY | XZ | XT | YZ | YT | ZT => Ok(Matrice::new(Some(axes))),
            _ => Err(BadArgType(format!("Cannot create a 2D matrix with {} dimensions!",
                                        axes)))
        }
    }

    pub fn new_3d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XYZ | XYT | XZT | YZT => Ok(Matrice::new(Some(axes))),
            _ => Err(BadArgType(format!("Cannot create a 3D matrix with {} dimensions!",
                                        axes)))
        }
    }

    pub fn new_4d(axes: Axes) -> CalcResult<Matrice> {
        match axes {
            XYZT => Ok(Matrice::new(Some(axes))),
            _ => Err(BadArgType(format!("Cannot create a 4D matrix with {} dimensions",
                                        axes)))
        }
    }

    pub fn extend_1d(&mut self, axis: Axes, other: Vec<BigRational>) -> CalcResult {
        if self.axes.is_none() {
            return Err(BadArgType("Cannot extend a nonexistent axis".to_str()))
        }

        match axis {
            X | Y | Z | T => { },
            _ => return Err(BadArgType("Not a single dimension!".to_str()))
        }

        if self.axes.unwrap() != axis {
            return Err(BadArgType("That axis is not yet defined for this matrix!".to_str()))
        }

        self.elems.push_all(other.as_slice());
        self.dimensionality = OneD(self.elems.len());
        Ok(Atom(Void))
    }
}

pub fn get_axes(arg: &ArgType, env: &mut Environment) -> CalcResult<Option<Axes>> {
    match try!(arg.arg_to_literal(env)) {
        Symbol(ref x) => Axes::from_str(x),
        _ => Err(BadArgType("Bad argument for axes".to_str()))
    }
}

pub fn make_matrix(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("`make-matrix' takes two arguments".to_str()))
    }

    let dimens_arg = match try!(args.get(0).desymbolize(env)) {
        BigNum(x) => match x.to_integer().to_u8() {
            Some(num) => num,
            None => return Err(BadDimensionality)
        },
        _ => return Err(BadDimensionality)
    };

    let axis_arg = try!(get_axes(args.get(1), env));

    if axis_arg == None && dimens_arg == 0 {
        return Ok(Atom(Matrix(Matrice::new_null())))
    } else if (axis_arg == None) ^ (dimens_arg == 0) {
        return Err(BadArgType("Tried to create a non-null matrix with no axes".to_str()))
    }

    let matrix = match dimens_arg {
        1 => try!(Matrice::new_1d(axis_arg.unwrap())),
        2 => try!(Matrice::new_2d(axis_arg.unwrap())),
        3 => try!(Matrice::new_3d(axis_arg.unwrap())),
        4 => try!(Matrice::new_4d(axis_arg.unwrap())),
        _ => return Err(BadArgType("Idk yet".to_str()))
    };

    Ok(Atom(Matrix(matrix)))
}

pub fn matrix_extend(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`matrix-extend' takes three arguments".to_str()))
    }

    let mut matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_str()))
    };

    let axes = match try!(get_axes(args.get(1), env)) {
        None => return Err(BadArgType("Cannot extend a non-axis".to_str())),
        Some(x) => x,
    };

    let other = match try!(args.get(2).desymbolize(env)) {
        List(list) => {
            let mut arg_list = Vec::new();
            for x in list.move_iter() {
                match x {
                    BigNum(n) => arg_list.push(n),
                    Symbol(s) => match try!(env.lookup(&s)) {
                        BigNum(n) => arg_list.push(n),
                        _ => return Err(BadArgType("Matrices only take numbers".to_str()))
                    },
                    _ => return Err(BadArgType("Matrices only take numbers".to_str()))
                }
            }
            arg_list
        }
        _ => return Err(BadArgType("Elements to extend a matrix must be given in a list".to_str()))
    };

    match matrix.dimensionality {
        OneD(_) => { try!(matrix.extend_1d(axes, other)); },
        _ => { }
    };

    Ok(Atom(Matrix(matrix)))
}
