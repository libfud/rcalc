//! Operators

extern crate types;

pub use super::num::{zero, one};
use self::types::{CalcResult, Env};
use self::types::operator::*;
use self::types::sexpr::ArgType;
use self::types::sexpr::ArgType::Atom;
use super::matrice::matrix_ops;
use self::types::literal::LiteralType::Void;

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

#[inline]
pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, env: &mut Env) -> CalcResult {
    match op_type {
        OperatorType::Quote            =>             Ok(Atom(Void)),
        OperatorType::Lambda           =>             Ok(Atom(Void)),
        OperatorType::Define           =>      super::define(args, env),
        OperatorType::Help             =>      super::common::help(args),
        OperatorType::RecOps(rop)      =>      super::record::record_ops(args, env, rop),
        OperatorType::MatrixStuff(mop) =>      super::matrice::matrix_ops(args, env, mop),
        OperatorType::Transcend(op)    =>       trig::float_ops(args, env, op),
        OperatorType::Pow              =>      power::pow_wrapper(args, env),
        OperatorType::Ordering(ord)    =>      logic::ordering(args, env, ord),
        OperatorType::RoundIdent(ri)   =>      logic::num_op(args, env, ri),
        OperatorType::Logic(gate)      =>      logic::handle_logic(args, env, gate),
        OperatorType::Query(qop)       =>      logic::query(args, env, qop),
        OperatorType::Listings(lop)    =>    listops::list_ops(args, env, lop),
        OperatorType::TransForms(top)  =>    listops::transform_ops(args, env, top),
        OperatorType::Table            =>    special::table(args, env),
        OperatorType::TableFromMatrix  =>    special::table_from_matrix(args, env),
        OperatorType::Graph            =>    special::graph(args, env),
        OperatorType::Arithmetic(op)   => arithmetic::arith(args, env, op),
    }
}
