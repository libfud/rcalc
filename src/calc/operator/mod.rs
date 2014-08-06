//! Operators

extern crate types;

pub use super::{Args, Atom, CalcResult, SExpr, Env, Environment, Void};
use self::types::operator::*;
use super::matrice::matrix_ops;

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

#[inline]
pub fn eval(op_type: OperatorType, args: &Args, env: &mut Env) -> CalcResult {
    match op_type {
        Quote            =>             Ok(Atom(Void)),
        Lambda           =>             Ok(Atom(Void)),
        Define           =>      super::define(args, env),
        Help             =>      super::common::help(args),
        RecOps(rop)      =>      super::record::record_ops(args, env, rop),
        MatrixStuff(mop) =>      super::matrice::matrix_ops(args, env, mop),
        Transcend(op)    =>       trig::float_ops(args, env, op),
        Pow              =>      power::pow_wrapper(args, env),
        Ordering(ord)    =>      logic::ordering(args, env, ord),
        RoundIdent(ri)   =>      logic::num_op(args, env, ri),
        Logic(gate)      =>      logic::handle_logic(args, env, gate),
        Query(qop)       =>      logic::query(args, env, qop),
        Listings(lop)    =>    listops::list_ops(args, env, lop),
        TransForms(top)  =>    listops::transform_ops(args, env, top),
        Table            =>    special::table(args, env),
        TableFromMatrix  =>    special::table_from_matrix(args, env),
        Graph            =>    special::graph(args, env),
        Arithmetic(op)   => arithmetic::arith(args, env, op),
    }
}
