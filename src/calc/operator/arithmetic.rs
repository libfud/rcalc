//!Basic arithemtic functions. 

extern crate types;

use std::num;
use self::types::operator::{Add, Sub, Mul, Div, Rem, OperatorType};
use super::super::{CalcResult, Environment, BadNumberOfArgs, Evaluate};
use super::super::{BigNum};
use super::{BigRational, ArgType, Atom, Lit, LitRes};

pub type Args<T = ArgType> = Vec<T>;
pub type BigR = BigRational;
pub type Env = Environment;

fn minlen_op_ident(op: OperatorType) -> (uint, |Lit, &Lit| -> Lit,  BigR) {
    match op {
        Add => (0, |a: Lit, b: &Lit| a + *b, num::zero()),
        Sub => (1, |a: Lit, b: &Lit| a - *b, num::zero()),
        Mul => (0, |a: Lit, b: &Lit| a * *b, num::one()),
        Div => (1, |a: Lit, b: &Lit| a / *b, num::one()),
        Rem => (1, |a: Lit, b: &Lit| a % *b, num::one()),
        _ => fail!("Don't do that")
    }
}

pub fn arith(args: &Args, env: &mut Env, oper: OperatorType) -> CalcResult {
    let (min_len, op, ident) = minlen_op_ident(oper);

    if args.len() < min_len {
        return Err(BadNumberOfArgs(format!(
            "Specified operation requires at least {} arguments", min_len)))
    }

    let ident = BigNum(ident);

    if args.len() == 1 {
        Ok(Atom(op(ident, &try!(args.get(0).desymbolize(env)))))
    } else {
        let first = try!(args.get(0).desymbolize(env));
        answer = args.tail().iter().fold(first, |a, b|, op(a, b))
        for x in args.tail().iter() {
            answer = op(answer, &try!(x.desymbolize(env)));
        }
        Ok(Atom(answer))
    }
}

