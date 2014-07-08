//! Operators

extern crate types;
extern crate num;

pub use self::num::bigint;
pub use super::{BigRational, Ratio, CalcResult, Environment, ArgType, Atom, SExpr};
pub use super::{LiteralType, Lit, LitRes, Symbol, Void};
pub use super::literal::{cons, car, cdr, list};
pub use self::types::operator::{OperatorType, Arithmetic, Transcend, Ordering,
                                Pow, RoundIdent, Logic, Quote, Listings, ListOps,
                                TransForms, XForms, Define, Lambda, Table, 
                                Gate, MatrixStuff, MatrixOps, Help};
use super::matrice;

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

pub fn list_ops(args: &Vec<ArgType>, env: &mut Environment, lop: ListOps) -> CalcResult {
    use self::types::operator::{List, Cons, Car, Cdr, Cadr, Cddr, Caddr, Cdddr};
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

pub fn transform_ops(args: &Vec<ArgType>, env: &mut Environment, top: XForms) -> CalcResult {
    use self::types::operator::{Map, Reduce, Filter, RangeList, Sort};
    use self::listops::{map, filter, reduce, rangelist};
    use self::special::sort;

    match top {
        Map => map(args, env),
        Reduce => reduce(args, env),
        Filter => filter(args, env),
        RangeList => rangelist(args, env), 
        Sort => sort(args, env),
    }
}

pub fn handle_logic(args: &Vec<ArgType>, env: &mut Environment, log: Gate) -> CalcResult {
    use self::types::operator::{If, And, Or, Not, Xor};
    use self::logic::{and_or, not, xor};
    match log {
        If  => logic::cond(args, env),
        And => and_or(args, env, false), 
        Or  => and_or(args, env, true),
        Not => not(args, env), 
        Xor => xor(args, env),
    }
}

pub fn matrix_stuff(args: &Vec<ArgType>, env: &mut Environment, mop: Matrix) -> CalcResult {
    use self::types::operator::{MatrixSet, MatrixExtend, MakeMatrix};    

    match mop {
        MatrixSet => matrice::matrix_set(args, env),
        MakeMatrix => matrice::make_matrix(args, env),
        MatrixExtend => matrice::matrix_extend(args, env),
    }
}        

pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, 
            env: &mut Environment) -> CalcResult {

    use self::arithmetic::arith;
    use self::special::{table};
    use self::logic::{ordering, num_op};
    use self::trig::float_ops;


    match op_type {
        Arithmetic(op) => arith(args, env, op),
        Pow => power::pow_wrapper(args, env),
        Transcend(op) => float_ops(args, env, op),
        Ordering(ord) => ordering(args, env, ord.to_ord()),
        RoundIdent(ri) => num_op(args, env, ri),
        Logic(gate) => handle_logic(args, env, gate),
        Define  => super::define(args, env),
        Lambda => Ok(Atom(Void)),
        Quote => Ok(Atom(Void)),
        Listings(lop) => list_ops(args, env, lop),
        TransForms(top) => transform_ops(args, env, top),
        Table => special::table(args, env),
        MatrixStuff(mop) => matrix_stuff(args, env, mop),
        Help => super::common::help(args),
    }
}
