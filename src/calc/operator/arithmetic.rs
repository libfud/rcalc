//!Basic arithemtic functions. 

use super::super::{CalcResult, Environment, BadNumberOfArgs};
use super::super::literal::{BigNum};
use super::{BigRational, ArgType, Atom, Lit, LitRes};

pub type Args<T = ArgType> = Vec<T>;
pub type BigR = BigRational;
pub type Env = Environment;

pub fn arith(args: &Args, env: &mut Env, min_len: uint, 
             op: |Lit, &Lit| -> LitRes, ident_fn: || -> BigR) -> CalcResult {
    if args.len() < min_len {
        return Err(BadNumberOfArgs(format!(
            "Specified operation requires at least {} arguments", min_len)))
    }

    let ident = BigNum(ident_fn());

    if args.len() == 1 {
        Ok(Atom(try!(op(ident, &try!(args.get(0).desymbolize(env))))))
    } else {
        let mut answer = try!(args.get(0).desymbolize(env));
        for x in args.tail().iter() {
            answer = try!(op(answer, &try!(x.desymbolize(env))));
        }
        Ok(Atom(answer))
    }
}

