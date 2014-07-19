//! Matrices

extern crate matrix;
extern crate types;

use self::matrix::{Matrice, MatrixErrors, BadDimensionality};
use self::types::MatrixErr;
use self::types::operator::{MatrixOps, MakeMatrix, MatrixSetRow, MatrixSetCol, 
                            MatrixAppendRows, Determ, MatrixInv, MatrixAppendCols,
                            MatrixGetElem, MatrixGetRow, MatrixGetCol, MatrixFromFn};
use super::{ArgType, Atom, CalcResult, Environment, Evaluate};
use super::{BadArgType, BadNumberOfArgs};
use super::{Lit, List, BigNum, Matrix, Symbol};

type Env<T = Environment> = T;
type Args<T = ArgType> = Vec<T>;

pub fn matrix_ops(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    match mop {
        MakeMatrix => make_matrix(args, env),
        MatrixSetRow | MatrixSetCol => matrix_set(args, env, mop),
        MatrixAppendRows | MatrixAppendCols => matrix_append(args, env, mop),
        MatrixGetElem => get_elem(args, env),
        MatrixGetRow | MatrixGetCol => get_row_col(args,env, mop),
        Determ | MatrixInv => single(args, env, mop),
        MatrixFromFn => matrix_from_fn(args, env),
    }
}        

pub fn make_matrix(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() > 1  {
        return Err(BadNumberOfArgs("make-matrix".to_string(), "at most".to_string(), 1))
    }

    let matrix_res: Result<Matrice<Lit>, MatrixErrors> = if args.len() == 0 {
        Ok(Matrice::new())
    } else {
        let (elems, (x, y)) = try!(list_to_2d(try!(args[0].desymbolize(env)), env));
        Matrice::from_vec(elems, x, y) 
    };

    match matrix_res {
        Ok(x) => Ok(Atom(Matrix(x))),
        Err(m) => Err(MatrixErr(m))
    }
}

pub fn matrix_from_fn(args: &Args, env: &mut Env) -> CalcResult {
    use super::operator::listops::proc_getter;

    if args.len() < 2 {
        return Err(BadNumberOfArgs("matrix-from-fn".to_string(), 
                                   "at least".to_string(), 2))
    }

    let (names, func) = try!(proc_getter(args, env));
    if names.len() < 1 {
        return Err(BadArgType("At least one variable must be supplied".to_string()))
    }

    if args.len() - 1 != names.len() {
        return Err(BadNumberOfArgs(func.to_string(), "only".to_string(), 
                                   names.len()))
    }

    let mut lists: Vec<Vec<Lit>> = Vec::with_capacity(args.tail().len());
    for arg in args.tail().iter() {
        match try!(arg.desymbolize(env)) {
            List(x) => lists.push(x),
            _ => return Err(BadArgType("Arguments to function given as lists.".to_string()))
        }
    }

    if lists.tail().iter().any(|x| x.len() != lists[0].len()) {
        return Err(BadArgType("Each list of arguments must be the same length".to_string()))
    }

    let mut matrix_vec: Vec<Lit> = Vec::new();

    for column in range(0, lists[0].len()) {
        let mut child_env = Environment::new_frame(env);

        let values: Vec<Lit> = lists.iter().map(|x| x[column].clone()).collect();
        matrix_vec.push_all(values.as_slice());

        for (arg, val) in names.iter().zip(values.iter()) {
            child_env.symbols.insert(arg.clone(), val.clone());
        }

        matrix_vec.push(try!(try!(func.eval(&mut child_env)).desymbolize(env)));
    }

    match Matrice::from_vec(matrix_vec, lists.len() + 1, lists[0].len()) {
        Ok(x) => Ok(Atom(Matrix(x))),
        Err(m) => Err(MatrixErr(m))
    }
}

pub fn arg_to_uint(arg: Lit) -> CalcResult<uint> {
    match arg {
        BigNum(x) => match x.to_integer().to_uint() {
            Some(num) => Ok(num),
            None => Err(BadArgType("Number must be a positive integer".to_string()))
        },
        _ => Err(BadArgType("Number must be a positive integer".to_string()))
    }
}

pub fn get_elem(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("matrix-get-elem".to_string(), "only".to_string(), 3))
    }

    let matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    let mut row = try!(arg_to_uint(try!(args[1].desymbolize(env))));
    let mut col = try!(arg_to_uint(try!(args[2].desymbolize(env))));

    if row == 0 || col == 0 {
        return Err(BadArgType("Matrices are indexed starting from 1".to_string()))
    }

    row -= 1;
    col -= 1;

    match matrix.get_elem(row, col) {
        Some(x) => Ok(Atom(x)),
        None => Err(MatrixErr(BadDimensionality))
    }
}

pub fn get_row_col(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 2))
    }

    let matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        y => return Err(BadArgType(format!("{} is not a matrix", y)))
    };

    let mut row_col = try!(arg_to_uint(try!(args[1].desymbolize(env))));

    if row_col == 0 {
        return Err(BadArgType("Matrices are indexed starting from 1".to_string()))
    }

    row_col -= 1;

    match mop {
        MatrixGetRow => if row_col > matrix.rows() {
            Err(MatrixErr(BadDimensionality))
        } else {
            Ok(Atom(List(matrix.get_row(row_col).map(|x| x.clone()).collect())))
        },
        MatrixGetCol => if row_col > matrix.cols() {
            Err(MatrixErr(BadDimensionality))
        } else {
            Ok(Atom(List(matrix.get_col(row_col).map(|x| x.clone()).collect())))
        },
        _ => fail!("Undefined")
    }
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

pub fn matrix_set(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 3))
    }

    let mut matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    let old_item = try!(arg_to_uint(try!(args[1].desymbolize(env))));
 
    let (new_items, _) = try!(list_to_1d(try!(args[2].desymbolize(env)), env));

    match mop {
        MatrixSetRow => match matrix.set_row(old_item, new_items) {
            Ok(_) => Ok(Atom(Matrix(matrix))),
            Err(m) => Err(MatrixErr(m))
        },
        MatrixSetCol => match matrix.set_col(old_item, new_items) {
            Ok(_) => Ok(Atom(Matrix(matrix))),
            Err(m) => Err(MatrixErr(m))
        },
        _ => fail!("Undefined")
    }
}

pub fn matrix_append(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 2))
    }

    let mut matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    let (new_items,(len, count)) = try!(list_to_2d(try!(args[1].desymbolize(env)), env));

    match mop {
        MatrixAppendRows => {
            for list in range(0, count) {
                match matrix.append_row(new_items.slice(list * len, (list + 1) * len).to_owned()) {
                    Ok(_) =>  { },
                    Err(m) => return Err(MatrixErr(m))
                }
            }
        }
        MatrixAppendCols => {
            for list in range(0, count) {
                match matrix.append_col(new_items.slice(list * len, (list + 1) * len).to_owned()) {
                    Ok(_) =>  { },
                    Err(m) => return Err(MatrixErr(m))
                }
            }
        }
        _ => fail!("Undefined")
    }

    Ok(Atom(Matrix(matrix)))
}

pub fn single(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 1))
    }

    let matrix = match try!(args[0].desymbolize(env)) {
        Matrix(x) => x.clone(),
        _ => return Err(BadArgType("Not a matrix".to_string()))
    };

    match mop {
        Determ => match matrix.determinant() {
            Some(x) => Ok(Atom(x)),
            None =>  Err(BadArgType("No determinant for this matrix".to_string()))
        },
        MatrixInv =>     match matrix.inverse() {
            Some(x) => Ok(Atom(Matrix(x))),
            None =>  Err(BadArgType("No determinant for this matrix".to_string()))
        },
        _ => fail!("Undefined!")
    }
}
