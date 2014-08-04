//! Matrices

extern crate matrix;
extern crate types;

use self::matrix::{Matrice, MatrixErrors, BadDimensionality};
use self::types::MatrixErr;
use self::types::operator::*;
use self::types::literal::{Lit, List, Matrix};
use super::{Args, Env, Atom, CalcResult, Environment, Evaluate, BadArgType, BadNumberOfArgs};

#[inline]
pub fn matrix_ops(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    match mop {
        MakeMatrix => make_matrix(args, env),

        MatrixFromFn => matrix_from_fn(args, env),

        MatrixSetRow | MatrixSetCol => matrix_set(args, env, mop),

        MatrixAppendRows | MatrixAppendCols => matrix_append(args, env, mop),

        MatrixGetElem => get_elem(args, env),

        MatrixGetRow | MatrixGetCol => get_row_col(args,env, mop),

        Determ | MatrixInv | Transpose | PolygonArea => single(args, env, mop),

        Scalar => scalar(args, env),

        Translate => translate_by(args, env),

        DotProd | CrossProd | 
        MatrixConcatRows | MatrixConcatCols => double(args, env, mop),
    }
}        

pub fn list_to_2d(arg: Lit, env: &mut Env) -> CalcResult<(Vec<Lit>, (uint, uint))> {
    let mut length = 0u;
    let mut width = 0u;
    let list = try!(arg.to_vec());
    let mut arg_list = Vec::new();

    for x in list.move_iter() {
        let sub_list = try!(try!(Atom(x).desymbolize(env)).to_vec());
        length = sub_list.len();
        arg_list.push_all(sub_list.as_slice());
        width += 1;
    }

    Ok((arg_list, (length, width)))
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

/// Make a matrix from a function. Each list supplied to it represents different coefficients for an
/// additional variable. The number of lists given plus one represents the width of the matrix, and
/// the number of variables in the first list determines its height.
pub fn matrix_from_fn(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("matrix-from-fn".to_string(), "at least".to_string(), 2))
    }

    let (names, func) = try!(try!(args[0].desymbolize(env)).to_proc());
    if names.len() < 1 {
        return Err(BadArgType("At least one variable must be supplied".to_string()))
    } else if args.len() - 1 != names.len() {
        return Err(BadNumberOfArgs(func.to_string(), "only".to_string(), names.len()))
    }

    let mut lists: Vec<Vec<Lit>> = Vec::with_capacity(args.tail().len());
    for arg in args.tail().iter() {
        lists.push(try!(try!(arg.desymbolize(env)).to_vec()));
    }

    if lists.tail().iter().any(|x| x.len() != lists[0].len()) {
        return Err(BadArgType("Each list of arguments must be the same length".to_string()))
    }

    let mut matrix_vec: Vec<Lit> = Vec::new();

    for column in range(0, lists[0].len()) {
        println!("{}", column);
        let mut child_env = Environment::new_frame(env);

        let values: Vec<Lit> = lists.iter().map(|x| x[column].clone()).collect();
        matrix_vec.push_all(values.as_slice());

        for (arg, val) in names.iter().zip(values.iter()) {
            child_env.symbols.insert(arg.clone(), val.clone());
        }

        matrix_vec.push(try!(try!(func.eval(&mut child_env)).desymbolize(env)));
    }

    println!("{}", matrix_vec);

    match Matrice::from_vec(matrix_vec, lists.len() + 1, lists[0].len()) {
        Ok(x) => Ok(Atom(Matrix(x))),
        Err(m) => Err(MatrixErr(m))
    }
}

pub fn scalar(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs("matrix-scalar".to_string(), "only".to_string(), 2))
    }

    let matrix = try!((try!(args[0].desymbolize(env))).to_matrix());
    let (names, func) = try!(try!(args[1].desymbolize(env)).to_proc());

    if names.len() != 1 {
        return Err(BadArgType("Only one variable expected".to_string()))
    };

    let matrix_elems = matrix.to_vec();
    let mut new_elems = Vec::with_capacity(matrix_elems.len());

    for elem in matrix_elems.iter() {
        let mut child_env = Environment::new_frame(env);
        child_env.symbols.insert(names[0].clone(), elem.clone());
        new_elems.push(try!(try!(func.eval(&mut child_env)).desymbolize(env)));
    }

    match Matrice::from_vec(new_elems, matrix.cols(), matrix.rows()) {
        Ok(x) => Ok(Atom(Matrix(x))),
        Err(m) => Err(MatrixErr(m))
    }
}

pub fn get_elem(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs("matrix-get-elem".to_string(), "only".to_string(), 3))
    }

    let matrix = try!((try!(args[0].desymbolize(env))).to_matrix());
    let mut row = try!((try!(args[1].desymbolize(env))).to_uint());
    let mut col = try!((try!(args[2].desymbolize(env))).to_uint());

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

    let matrix = try!((try!(args[0].desymbolize(env))).to_matrix());
    let row_col = match try!((try!(args[1].desymbolize(env))).to_uint()) {
        0 => return Err(BadArgType("Matrices are indexed starting from 1".to_string())),
        x => x - 1
    };

    let (iterator, num) = match mop {
        MatrixGetRow => (matrix.get_row(row_col), matrix.rows()),
        MatrixGetCol => (matrix.get_col(row_col), matrix.cols()),
        _ => fail!("Undefined")
    };

    if row_col >  num {
        Err(MatrixErr(BadDimensionality))
    } else {
        Ok(Atom(List(iterator.map(|x| x.clone()).collect())))
    }
}

pub fn matrix_set(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 3 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 3))
    }

    let mut matrix = try!((try!(args[0].desymbolize(env))).to_matrix());
    let old_item = try!((try!(args[1].desymbolize(env))).to_uint());
    let new_items = try!(try!(args[2].desymbolize(env)).to_vec());

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

    let mut matrix = try!((try!(args[0].desymbolize(env))).to_matrix());
    let (new_items,(len, count)) = try!(list_to_2d(try!(args[1].desymbolize(env)), env));

    match mop {
        MatrixAppendRows => for list in range(0, count) {
            match matrix.append_row(new_items.slice(list * len, (list + 1) * len).to_vec()) {
                Ok(_) =>  { },
                Err(m) => return Err(MatrixErr(m))
            }
        },
        MatrixAppendCols => for list in range(0, count) {
            match matrix.append_col(new_items.slice(list * len, (list + 1) * len).to_vec()) {
                Ok(_) =>  { },
                Err(m) => return Err(MatrixErr(m))
            }
        },
        _ => fail!("Undefined")
    }

    Ok(Atom(Matrix(matrix)))
}

pub fn single(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 1))
    }

    let matrix = try!((try!(args[0].desymbolize(env))).to_matrix());

    let res = match mop {
        Determ => matrix.determinant(),
        MatrixInv => match matrix.inverse() {
            Some(x) => return Ok(Atom(Matrix(x))),
            None => return Err(BadArgType("No invsere for this matrix".to_string()))
        },
        Transpose => return Ok(Atom(Matrix(matrix.transpose()))),
        _ => matrix.polygon_area(),
    };

    match res {
        Some(x) => Ok(Atom(x)),
        None => Err(BadArgType(format!("{} failed", mop)))
    }
}

pub fn double(args: &Args, env: &mut Env, mop: MatrixOps) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs(mop.to_string(), "only".to_string(), 2))
    }

    let matrix_a = try!(try!(args[0].desymbolize(env)).to_matrix());
    let matrix_b = try!(try!(args[1].desymbolize(env)).to_matrix());
    let matrix = match mop {
        CrossProd => matrix_a.cross_prod(&matrix_b),
        DotProd => matrix_a.kronecker_prod(&matrix_b),
        MatrixConcatCols => matrix_a.concat_cols(&matrix_b),
        MatrixConcatRows | _ => matrix_a.concat_rows(&matrix_b) 
    };

    match matrix {
        Some(x) => Ok(Atom(Matrix(x))),
        None => Err(BadArgType("Mismatched matrices".to_string()))
    }
}

pub fn translate_by(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("matrix-translate".to_string(), "at least".to_string(), 2))
    }

    let matrix = try!(try!(args[0].desymbolize(env)).to_matrix());
    let mut translations = Vec::with_capacity(args.len() - 1);
    for arg in args.tail().iter() {
        translations.push(try!(arg.desymbolize(env)));
    }

    match matrix.translate_by(translations.as_slice()) {
        Some(x) => Ok(Atom(Matrix(x))),
        None => Err(BadArgType("Number of args does not match rows in matrix".to_string()))
    }
}
