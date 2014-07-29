//! Operators

extern crate types;

pub use super::{BigRational, Ratio, CalcResult, Environment, ArgType, Atom, SExpr};
pub use super::{LiteralType, Lit, LitRes, Symbol, Void};
pub use super::literal::{cons, car, cdr, list};
pub use self::types::operator::*;
use super::matrice;

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

pub type Env = Environment;

#[inline]
pub fn list_ops(args: &Vec<ArgType>, env: &mut Env, lop: ListOps) -> CalcResult {
    match lop {
        List => list(args, env),
        Cons => cons(args, env), 
        Car => car(args, env),
        Cdr => cdr(args, env),
        Cadr => car(&vec!(try!(cdr(args, env))), env), 
        Cddr => cdr(&vec!(try!(cdr(args, env))), env),
        Caddr => car(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
        Cdddr => cdr(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
    }
}

#[inline]
pub fn transform_ops(args: &Vec<ArgType>, env: &mut Env, top: XForms) -> CalcResult {
    use self::listops::{map, filter, fold, reduce, rangelist};
    use self::special::{sort, sort_by};

    match top {
        Map => map(args, env),
        Reduce => reduce(args, env),
        Fold | 
        FoldR => fold(args, env, top),
        Filter => filter(args, env),
        RangeList => rangelist(args, env), 
        Sort => sort(args, env),
        SortBy => sort_by(args, env),
    }
}

#[inline]
pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    use self::arithmetic::arith;
    use self::special::{table};
    use self::logic::{handle_logic, ordering, num_op};
    use self::trig::float_ops;

    match op_type {
        Arithmetic(op) => arith(args, env, op),
        Pow => power::pow_wrapper(args, env),
        Transcend(op) => float_ops(args, env, op),
        Ordering(ord) => ordering(args, env, ord),
        RoundIdent(ri) => num_op(args, env, ri),
        Logic(gate) => handle_logic(args, env, gate),
        Define  => super::define(args, env),
        Lambda => Ok(Atom(Void)),
        Quote => Ok(Atom(Void)),
        Listings(lop) => list_ops(args, env, lop),
        TransForms(top) => transform_ops(args, env, top),
        Table => special::table(args, env),
        TableFromMatrix => special::table_from_matrix(args, env),
        TextGraph => special::text_graph(args, env),
        MatrixStuff(mop) => matrice::matrix_ops(args, env, mop),
        RecOps(rop) => super::record::record_ops(args, env, rop),
        Help => super::common::help(args),
    }
}
