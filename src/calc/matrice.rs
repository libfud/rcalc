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

pub fn make_matrix(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    if args.len() > 1  {
        return Err(BadNumberOfArgs("`make-matrix' takes at most one argument".to_str()))
    }

    let matrix_res: Result<Matrice<Lit>, MatrixErrors> = if args.len() == 0 {
        let empty: Vec<Lit> = Vec::new();
        let wat: Result<Matrice<Lit>, MatrixErrors> = Matrice::new(empty, 0, 0);
        wat
    } else {
        let (elems, (x, y)) = try!(list_to_2d(try!(args.get(0).desymbolize(env)), env));
        Matrice::new(elems, x, y) 
    };

    let matrix = match matrix_res {
        Ok(x) => x,
        Err(m) => return Err(MatrixErr(m))
    };

    Ok(Atom(Matrix(matrix)))
}

pub fn list_to_1d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<Lit>, uint)> {
    match arg {
        List(list) => {
            let len = list.len();
            Ok((list, len))
        }
        Symbol(ref s) => list_to_1d(try!(env.lookup(s)), env),
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_str()))
    }
}

pub fn list_to_2d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<Lit>, (uint, uint))> {
    let mut length = 0u;
    let mut width = 0u;
    match arg {
        List(list) => {
            let mut arg_list = Vec::new();
            for x in list.move_iter() {
                match try!(Atom(x).desymbolize(env)) {
                    List(y) => {
                        let (sub_list, _) = try!(list_to_1d(List(y), env));
                        length = sub_list.len();
                        arg_list.push_all(sub_list.as_slice());
                    },
                    _ => return Err(BadArgType("Matrices only take numbers".to_str()))
                }
                width += 1;
            }
            Ok((arg_list, (length, width)))
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

    Ok(Atom(Matrix(matrix)))
}
