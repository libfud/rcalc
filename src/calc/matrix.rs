//! Matrices

use super::Matrice;
use super::{ArgType, Atom, BigRational, CalcResult, Environment};
use super::{BadArgType, BadNumberOfArgs, MismatchedAxes, 
            BadDimensionality, NullAxisExtension};
use super::literal::{Lit, BigNum, List, Matrix, Symbol, Void};

type BR<T = BigRational> = T;
type Env<T = Environment> = T;
type Args<T = ArgType> = Vec<T>;

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

pub fn matrix_set(args: &Args, env: &mut Env) -> CalcResult {
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
        TwoD(_, _) => { try!(matrix.set_2d(axes, other, other_d)); },
        _ => return Err(BadArgType("Not defined yet".to_str()))

    }

    Ok(Atom(Matrix(matrix)))
}
