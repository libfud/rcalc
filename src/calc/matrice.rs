//! Matrices

extern crate matrix;
extern crate types;

use self::matrix::{Matrice, MatrixErrors};
use self::types::MatrixErr;
use self::types::operator::{MatrixOps, MatrixSet, MakeMatrix, MatrixExtend, Determ, MatrixInv};
use super::{ArgType, Atom, CalcResult, Environment, Evaluate};
use super::{BadArgType, BadNumberOfArgs};
use super::{Lit, List, Matrix, Symbol};

type Env<T = Environment> = T;
type Args<T = ArgType> = Vec<T>;

pub fn matrix_ops(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    match mop {
        MatrixSet => matrix_set(args, env),
        MakeMatrix => make_matrix(args, env),
        MatrixExtend => matrix_extend(args, env),
/*        Determ => (args, env, |&a| Matrice::determinant(a)),
        MatrixInv => self_method(args, env, |&a| Matrice::inverse(a)),
*/
        Determ => determ(args, env),
        MatrixInv => invert(args, env)
    }
}        

pub fn make_matrix(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() > 1  {
        return Err(BadNumberOfArgs("`make-matrix' takes at most one argument".to_string()))
    }

    let matrix_res: Result<Matrice<Lit>, MatrixErrors> = if args.len() == 0 {
        Ok(Matrice::new())
    } else {
        let (elems, (x, y)) = try!(list_to_2d(try!(args.get(0).desymbolize(env)), env));
        Matrice::from_vec(elems, x, y) 
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
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_string()))
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
                    _ => return Err(BadArgType("Matrices only take numbers".to_string()))
                }
                width += 1;
            }
            Ok((arg_list, (length, width)))
        }
        _ =>  Err(BadArgType("Elements to extend a matrix must be given in a list".to_string()))
    }
}

pub fn matrix_extend(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`matrix-extend' takes three arguments".to_string()))
    }

    let matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    Ok(Atom(Matrix(matrix)))
}

pub fn matrix_set(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("`matrix-set' takes three arguments".to_string()))
    }

    let matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    Ok(Atom(Matrix(matrix)))
}

pub fn determ(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("`determinant takes one argument".to_string()))
    }

    let matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    match matrix.determinant() {
        Some(x) => Ok(Atom(x)),
        None =>  Err(BadArgType("No determinant for this matrix".to_string()))
    }
}

pub fn invert(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("`determinant takes one argument".to_string()))
    }

    let matrix = match try!(args.get(0).desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    match matrix.inverse() {
        Some(x) => Ok(Atom(Matrix(x))),
        None =>  Err(BadArgType("No determinant for this matrix".to_string()))
    }
}
