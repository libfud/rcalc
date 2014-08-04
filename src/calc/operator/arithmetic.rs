//!Basic arithemtic functions. 

extern crate types;

use std::num;
use self::types::operator::{Add, Sub, Mul, Div, Rem, Arith};
use super::super::{Args, Env, Atom, Lit, CalcResult, BadNumberOfArgs, Evaluate};

#[inline]
fn minlen_op_ident(op: &Arith) -> (uint, Option<uint>, |Lit, Lit| -> Lit,  Lit) {
    match op {
        &Add => (0, None, |a: Lit, b: Lit| a + b, num::zero()),
        &Sub => (1, None, |a: Lit, b: Lit| a - b, num::zero()),
        &Mul => (0, None, |a: Lit, b: Lit| a * b, num::one()),
        &Div => (1, None, |a: Lit, b: Lit| a / b, num::one()),
        &Rem => (2, Some(2), |a: Lit, b: Lit| a % b, num::one()),
    }
}

#[inline]
pub fn arith(args: &Args, env: &mut Env, oper: Arith) -> CalcResult {
    let (min_len, max_len, op, ident) = minlen_op_ident(&oper);
    if args.len() < min_len {
        return Err(BadNumberOfArgs(oper.to_string(), "at least".to_string(), min_len))
    } else if max_len.is_some() {
        let max = max_len.unwrap();
        if args.len() > max {
            return Err(BadNumberOfArgs(oper.to_string(), "at most".to_string(), max))
        }
    }

    match args.len() {
        0 => Ok(Atom(ident)),
        1 => Ok(Atom(op(ident, try!(args[0].desymbolize(env))))),
        _ => {
            let mut answer = try!(args[0].desymbolize(env));
            for x in args.tail().iter() {
                answer = op(answer, try!(x.desymbolize(env)));
            }
            Ok(Atom(answer))
        }
    }
}
