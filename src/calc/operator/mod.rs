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
        Arithmetic(op)   => arithmetic::arith(args, env, op),
        Pow              =>      power::pow_wrapper(args, env),
        Transcend(op)    =>       trig::float_ops(args, env, op),
        Ordering(ord)    =>      logic::ordering(args, env, ord),
        RoundIdent(ri)   =>      logic::num_op(args, env, ri),
        Logic(gate)      =>      logic::handle_logic(args, env, gate),
        Define           =>      super::define(args, env),
        Lambda           =>             Ok(Atom(Void)),
        Quote            =>             Ok(Atom(Void)),
        Listings(lop)    =>    listops::list_ops(args, env, lop),
        TransForms(top)  =>    listops::transform_ops(args, env, top),
        Table            =>    special::table(args, env),
        TableFromMatrix  =>    special::table_from_matrix(args, env),
        TextGraph        =>    special::text_graph(args, env),
        MatrixStuff(mop) =>             matrix_ops(args, env, mop),
        RecOps(rop)      =>      super::record::record_ops(args, env, rop),
        Query(qop)       =>      logic::query(args, env, qop),
        Help             =>      super::common::help(args),
    }
}
