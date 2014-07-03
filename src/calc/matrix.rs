//! Matrices

use std::fmt;
use super::{ArgType, Atom, BigRational, CalcResult, Environment};
use super::{BadArgType, BadNumberOfArgs, MismatchedAxes, 
            BadDimensionality, NullAxisExtension};
use super::literal::{Lit, BigNum, List, Matrix, Symbol, Void};

type BR<T = BigRational> = T;
type Env<T = Environment> = T;
type Args<T = ArgType> = Vec<T>;

#[deriving(Show, Clone, PartialOrd, PartialEq)]
pub enum Dimensionality {
    Null,
    OneD(uint),
    TwoD(uint, uint),
    ThreeD(uint, uint, uint),
    FourD(uint, uint, uint, uint),
}

type Dim<T = Dimensionality> = T;

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


#[deriving(Clone, PartialOrd, PartialEq)]
pub struct Matrice {
    dimensionality: Dimensionality,
    axes: Option<Axes>,
    elems: Vec<BR>,
}

impl fmt::Show for Matrice {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!("{} {} axes defined", self.dimensionality, match self.axes {
            Some(x) => x.to_str(),
            None => "no".to_str(),
        });
        match self.dimensionality {
            Null => { },
            OneD(_) => println!("{}", self.elems),
            TwoD(x, y) => {
                for row in range(0, x) {
                    print!("[");
                    for column in range(0, y) {
                        print!("{} ", self.elems.get(row + (column * row)))
                    }
                    print!("]");
                }
            }
            _ => println!("I don't know yet.\n{}", self.elems),
        }
        Ok(())
    }
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


    pub fn extend_1d(&mut self, axis: Axes, other: Vec<BR>) -> CalcResult {
        if self.axes.is_none() {
            return Err(NullAxisExtension)
        }

        match axis {
            X | Y | Z | T => { },
            _ => return Err(BadArgType("Not a single dimension!".to_str()))
        }

        if self.axes.unwrap() != axis {
            return Err(MismatchedAxes)
        }

        self.elems.push_all(other.as_slice());
        self.dimensionality = OneD(self.elems.len());
        Ok(Atom(Void))
    }

    pub fn extend_2d(&mut self, axes: Axes, other: Vec<BR>, other_dim: Dim) -> CalcResult {
        println!("Called extend_2d");
        if self.axes.is_none() {
            return Err(NullAxisExtension)
        }

        match axes {
            XY | XZ | XT | YZ | YT | ZT => { },
            _ => return Err(BadArgType(format!("{} is not two dimensions", axes)))
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

        if other_wid != width {
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
        Ok(Atom(Void))
    }
}

pub fn get_axes(arg: Lit) -> CalcResult<Option<Axes>> {
    match arg {
        Symbol(ref x) => Axes::from_str(x),
        _ => Err(BadArgType("Bad argument for axes".to_str()))
    }
}

pub fn get_dimens(arg: Lit) -> CalcResult<u8> {
    match arg {
        BigNum(x) => match x.to_integer().to_u8() {
            Some(num) => Ok(num),
            None => Err(BadDimensionality)
        },
        _ => return Err(BadArgType("Dimensionality must be specified as a number".to_str()))
    }
}

pub fn make_matrix(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("`make-matrix' takes two arguments".to_str()))
    }

    let dimens_arg = try!(get_dimens(try!(args.get(0).desymbolize(env))));
    let axis_arg = try!(get_axes(try!(args.get(1).arg_to_literal(env))));

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

pub fn list_to_1d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<BR>, Dim)> {
    match arg {
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
            let len = arg_list.len();
            Ok((arg_list, OneD(len)))
        }
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_str()))
    }
}

pub fn list_to_2d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<BR>, Dim)> {
    let mut length = 0u;
    match arg {
        List(list) => {
            let mut arg_list = Vec::new();
            for x in list.move_iter() {
                match x {
                    List(_) => {
                        let (sub_list, _) = try!(list_to_1d(x, env));
                        length = sub_list.len();
                        arg_list.push_all(sub_list.as_slice());
                    },

                    Symbol(s) => {
                        let res = try!(env.lookup(&s));
                        match res {
                            List(_) => {
                                let (sub_list, _) = try!(list_to_1d(res, env));
                                length = sub_list.len();
                                arg_list.push_all(sub_list.as_slice());
                            }

                            _ => return Err(BadArgType("Matrices only take numbers".to_str()))
                        }
                    },

                    _ => return Err(BadArgType("Matrices only take numbers".to_str()))
                }
            }
            let wid = arg_list.len();
            Ok((arg_list, TwoD(length, wid)))
        }
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_str()))
    }
}

pub fn matrix_extend(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`matrix-extend' takes three arguments".to_str()))
    }

    let mut matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_str()))
    };

    let axes = match try!(get_axes(try!(args.get(1).arg_to_literal(env)))) {
        Some(x) => x,
        None => return Err(NullAxisExtension)
    };

    let list = try!(args.get(2).desymbolize(env));

    let (other, other_d)  = match axes {
        X | Y | Z | T => try!(list_to_1d(list, env)),
        XY | XZ | XT | YZ | YT | ZT => try!(list_to_2d(list, env)),
        _ => return Err(BadArgType("Not defined yet".to_str()))
    };

    match matrix.dimensionality {
        OneD(_) => { try!(matrix.extend_1d(axes, other)); },
        TwoD(_, _) => { try!(matrix.extend_2d(axes, other, other_d)); },
        _ => return Err(BadArgType("Not defined yet".to_str()))

    }

    Ok(Atom(Matrix(matrix)))
}
