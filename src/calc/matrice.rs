//! Matrices

extern crate matrix;
extern crate types;

use self::matrix::*;
use self::types::MatrixErr;
use super::{ArgType, Atom, CalcResult, Environment, Evaluate};
use super::{BadArgType, BadNumberOfArgs};
use super::{Lit, List, Matrix, Symbol};

type Env<T = Environment> = T;
type Args<T = ArgType> = Vec<T>;

pub fn get_axes(arg: Lit) -> CalcResult<Option<Axes>> {
    match arg {
        Symbol(ref x) => match Axes::from_str(x) {
            Ok(axes) => Ok(axes),
            Err(m) => Err(MatrixErr(m)),
        },
        _ => Err(BadArgType("Bad argument for axes".to_str()))
    }
}

pub fn make_matrix(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("`make-matrix' takes one argument for axes".to_str()))
    }

    let axis_arg = try!(get_axes(try!(args.get(0).arg_to_literal(env))));

    let matrix: Matrice<Lit> = Matrice::new(axis_arg);

    Ok(Atom(Matrix(matrix)))
}

pub fn list_to_1d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<Lit>, Dim)> {
    match arg {
        List(list) => {
            let len = list.len();
            Ok((list, OneD(len)))
        }
        Symbol(ref s) => list_to_1d(try!(env.lookup(s)), env),
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_str()))
    }
}

pub fn list_to_2d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<Lit>, Dim)> {
    let mut length = 0u;
    let mut width = 0u;
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
                width += 1;
            }
            Ok((arg_list, TwoD(length, width)))
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
        None => return Err(MatrixErr(NullAxisExtension))
    };
    let list = try!(args.get(2).desymbolize(env));

    let (other, other_d)  = match axes {
        X | Y | Z | T => try!(list_to_1d(list, env)),
        XY | XZ | XT | YZ | YT | ZT => try!(list_to_2d(list, env)),
        _ => return Err(BadArgType("Not defined yet".to_str()))
    };

    match matrix.dimensionality {
        OneD(_) => match matrix.extend_1d(axes, other) {
            Err(m) => return Err(MatrixErr(m)),
            _ => { }
        },
        TwoD(_, _) => match matrix.extend_2d(axes, other, other_d) {
            Err(m) => return Err(MatrixErr(m)),
            _ => { }
        },
        _ => return Err(BadArgType("Not defined yet".to_str()))

    }

    Ok(Atom(Matrix(matrix)))
}

pub fn matrix_set(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`matrix-set' takes three arguments".to_str()))
    }

    let mut matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_str()))
    };

    let axes = match try!(get_axes(try!(args.get(1).arg_to_literal(env)))) {
        Some(x) => x,
        None => return Err(MatrixErr(NullAxisExtension))
    };

    let list = try!(args.get(2).desymbolize(env));

    let (other, other_d)  = match axes {
        X | Y | Z | T => try!(list_to_1d(list, env)),
        XY | XZ | XT | YZ | YT | ZT => try!(list_to_2d(list, env)),
        _ => return Err(BadArgType("Not defined yet".to_str()))
    };

    match matrix.dimensionality {
        OneD(_) => match matrix.extend_1d(axes, other) {
            Ok(()) => Ok(Atom(Matrix(matrix))),
            Err(m) => Err(MatrixErr(m))
        },
        TwoD(_, _) => match matrix.set_2d(axes, other, other_d) {
            Ok(()) => Ok(Atom(Matrix(matrix))),
            Err(m) => Err(MatrixErr(m)),
        },
        _ => Err(BadArgType("Not defined yet".to_str()))

    }
}

pub fn matrix_add(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("`matrix-add' requires at least two terms".to_str()))
    }

    let (a, b) = match (try!(args.get(0).desymbolize(env)),
                        try!(args.get(1).desymbolize(env))) {
        (Matrix(x), Matrix(y)) => (x, y),
        _ => return Err(BadArgType("Not matrices!".to_str()))
    };

    match a + b {
        Ok(x) => Ok(Atom(Matrix(x))),
        Err(m) => Err(MatrixErr(m))
    }
}
