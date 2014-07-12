//!Basic arithemtic functions. 

extern crate types;

use std::num;
use self::types::operator::{Add, Sub, Mul, Div, Rem, Arith};
use self::types::literal::Matrix;
use super::super::{CalcResult, Environment, BadNumberOfArgs, BadArgType, Evaluate};
use super::super::{BigNum};
use super::{BigRational, ArgType, Atom, Lit};

pub type Args<T = ArgType> = Vec<T>;
pub type BigR = BigRational;
pub type Env = Environment;

fn minlen_op_ident(op: &Arith) -> (uint, |Lit, &Lit| -> Lit,  BigR) {
    match op {
        &Add => (0, |a: Lit, b: &Lit| a + *b, num::zero()),
        &Sub => (1, |a: Lit, b: &Lit| a - *b, num::zero()),
        &Mul => (0, |a: Lit, b: &Lit| a * *b, num::one()),
        &Div => (1, |a: Lit, b: &Lit| a / *b, num::one()),
        &Rem => (1, |a: Lit, b: &Lit| a % *b, num::one()),
    }
}

pub fn arith(args: &Args, env: &mut Env, oper: Arith) -> CalcResult {
    let (min_len, op, ident) = minlen_op_ident(&oper);

    if args.len() < min_len {
        return Err(BadNumberOfArgs(format!(
            "Specified operation requires at least {} arguments", min_len)))
    }

    let ident = BigNum(ident);

    if args.len() == 1 {
        match oper {
            Sub => Ok(Atom(-try!(args.get(0).desymbolize(env)))),
            Div => match try!(args.get(0).desymbolize(env)) {
                Matrix(x) => match x.inverse() {
                        Some(inverted) => Ok(Atom(Matrix(inverted))),
                        None => Err(BadArgType("Inversion failed".to_string()))
                },
                x => Ok(Atom(op(ident, &x)))
            },                    
            _ => Ok(Atom(op(ident, &try!(args.get(0).desymbolize(env)))))
        }
    } else {
        let mut answer = try!(args.get(0).desymbolize(env));
        for x in args.tail().iter() {
            answer = op(answer, &try!(x.desymbolize(env)));
        }
        Ok(Atom(answer))
    }
}

