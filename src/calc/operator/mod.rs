//! Operators

use std::num;
pub use super::{BigRational, Ratio, bigint};
pub use super::{CalcResult, Environment, ArgType, Atom, SExpr};
pub use super::{LiteralType, Symbol, cons, car, cdr, list};

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, 
            env: &mut Environment) -> CalcResult {
    use self::logic::{num_op, and_or, not, xor, ordering};
    use self::arithmetic::arith;
    use self::listops::{map, filter, reduce, rangelist, listlen};
    use self::special::{table, sort};
    use self::trig::float_ops;
    use super::matrix;

    match op_type {
        Define  => super::define(args, env),
        Lambda => Ok(Atom(super::literal::Void)),

        List => list(args, env),
        Quote => Ok(Atom(super::literal::Void)),
        Cons => cons(args, env), 
        Car => car(args, env), Cdr => cdr(args, env),
        Cadr => car(&vec!(try!(cdr(args, env))), env), 
        Cddr => cdr(&vec!(try!(cdr(args, env))), env),
        Caddr => car(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
        Cdddr => cdr(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),

        Map => listops::map(args, env), Reduce => reduce(args, env),
        Filter => filter(args, env),

        Add | Sub | Mul | Div | Rem => {
            let (min, op, ident) = op_type.to_arith_args();
            arith(args, env, min, op, ident)
        },
        Pow => power::pow_wrapper(args, env),

        Sin | Cos | Tan | ASin |
        ACos | ATan | SinH | CosH | TanH | 
        ASinH | ACosH | ATanH | Log | Ln | Exp => float_ops(args, env, op_type),

        If   => logic::cond(args, env),
        And  => and_or(args, env, false), Or => and_or(args, env, true),
        Not  => not(args, env), Xor  => xor(args, env),

        Lt | LtEq | Eq | NEq | GtEq | Gt => ordering(args, env, op_type.to_ord()),

        Round | Ceiling | Floor | Zero | Even | Odd => num_op(args, env, op_type.to_numops()),

        Table => special::table(args, env),

        RangeList => rangelist(args, env), Sort => sort(args, env), 
        ListLen => listlen(args, env),

        MakeMatrix => matrix::make_matrix(args, env),
        MatrixExtend => matrix::matrix_extend(args, env),
        MatrixSet => matrix::matrix_set(args, env),

        Help => super::common::help(args),
    }
}
