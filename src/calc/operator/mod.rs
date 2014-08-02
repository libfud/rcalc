//! Operators

extern crate types;

pub use super::{ArgType, Args, Atom, CalcResult, SExpr, Env, Environment};
pub use self::types::literal::{Lit, Void};
use self::types::operator::*;
use super::matrice::matrix_ops;
use self::listops::{list_ops, transform_ops};

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

#[inline]
pub fn eval(op_type: OperatorType, args: &Args, env: &mut Env) -> CalcResult {
    use self::arithmetic::arith;
    use self::special::{table};
    use self::logic::{handle_logic, ordering, num_op, query};
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
        MatrixStuff(mop) => matrix_ops(args, env, mop),
        RecOps(rop) => super::record::record_ops(args, env, rop),
        Query(qop) => query(args, env, qop),
        Help => super::common::help(args),
    }
}
