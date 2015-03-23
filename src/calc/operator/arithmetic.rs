//!Basic arithemtic functions. 

extern crate types;

use super::{one, zero};
use self::types::operator::Arith::{Add, Sub, Mul, Div, Rem};
use self::types::operator::Arith;
use self::types::ArgType::Atom;
use self::types::ErrorKind::BadNumberOfArgs;
use super::super::{Args, Env, Lit, CalcResult, Evaluate};

#[inline]
fn minlen_op_ident(op: &Arith) -> (usize, Option<usize>, Box<FnMut(Lit, Lit) -> Lit>,  Lit) {
    match op {
        &Add => (0, None, Box::new(move |a: Lit, b: Lit| a + b), zero()),
        &Sub => (1, None, Box::new(move |a: Lit, b: Lit| a - b), zero()),
        &Mul => (0, None, Box::new(move |a: Lit, b: Lit| a * b), one()),
        &Div => (1, None, Box::new(move |a: Lit, b: Lit| a / b), one()),
        &Rem => (2, Some(2), Box::new(move |a: Lit, b: Lit| a % b), one()),
    }
}

#[inline]
pub fn arith(args: &Args, env: &mut Env, oper: Arith) -> CalcResult {
    let (min_len, max_len, mut op, ident) = minlen_op_ident(&oper);
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
