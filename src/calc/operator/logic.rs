//! Logic and odering.

extern crate types;

use self::types::sexpr::BuiltIn;
use self::types::literal::{Boolean, Lit};
use self::types::operator::*;
use super::super::{Evaluate, CalcResult, Environment, BadNumberOfArgs};
use super::{ArgType, Atom, SExpr};

pub type Args = Vec<ArgType>;
pub type Env = Environment;

#[inline]   
pub fn handle_logic(args: &Vec<ArgType>, env: &mut Env, log: Gate) -> CalcResult {
    match log {
        If  => cond(args, env),
        And => and_or(args, env, false), 
        Or  => and_or(args, env, true),
        Not => not(args, env), 
        Xor => xor(args, env),
    }
}

/// Loop through nested conditional statements until a non-conditional expression
/// is reached.
#[inline]
pub fn cond(args: &Args, env: &mut Env)  -> CalcResult {
    let mut arguments = args.clone();

    loop {
        if arguments.len() != 3 {
            return Err(BadNumberOfArgs("if".to_string(), "only".to_string(), 3))
        }

        let result = if try!(try!(arguments[0].desymbolize(env)).to_bool()) {
            arguments[1].clone()
        } else {
            arguments[2].clone()
        };

        match result {
            Atom(_) => return Ok(result),
            SExpr(ref x) => if x.expr_type == BuiltIn(Logic(If)) {
                arguments = x.args.clone();
            } else {
                return x.eval(env)
            }
        }
    }
}

#[inline]
pub fn ordering(args: &Args, env: &mut Env, ord: OrderEq) -> CalcResult {
    if args.len() != 2 {
        return Err(BadNumberOfArgs(ord.to_string(), "only".to_string(), 2))
    }

    Ok(Atom(Boolean(ord.to_ord()(try!(args[0].desymbolize(env)),
                         try!(args[1].desymbolize(env))))))
}

#[inline]
pub fn and_or(args: &Args, env: &mut Env, short: bool) -> CalcResult {    
    for val in args.iter() {
        if try!(val.desymbolize(env)) == Boolean(short) {
            return Ok(Atom(Boolean(short)))
        }
    }

    Ok(Atom(Boolean(!short)))
}

#[inline]
pub fn xor(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("xor".to_string(), "at least".to_string(), 2))
    }

    let mut result = try!(try!(args[0].desymbolize(env)).to_bool());
    for val in args.tail().iter() {
        result = result ^ try!(try!(val.desymbolize(env)).to_bool());
    }

    Ok(Atom(Boolean(result)))
}

#[inline]
pub fn not(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs("Not".to_string(), "only".to_string(), 1))
    }

    Ok(Atom(Boolean(!try!(try!(args[0].desymbolize(env)).to_bool()))))
}

pub fn num_op(args: &Args, env: &mut Env, op: RoundId) -> CalcResult {
    use std::num;
    use super::super::num::Integer;
    use self::types::operator::{Round, RoundToNearest, Floor, Ceiling, Zero};

    let one: Lit = num::one();
    let two = one + one;
    let half = one / two;

    if args.len() != 1 && op != RoundToNearest {
        return Err(BadNumberOfArgs(op.to_string(), "only".to_string(), 1))
    } else if args.len() != 2 && op == RoundToNearest {
        return Err(BadNumberOfArgs(op.to_string(), "only".to_string(), 2))
    }

    let num = try!(args[0].desymbolize(env));

    match op { 
        Round => Ok(Atom(try!(num.round()))),
        RoundToNearest => { 
            let nearest= try!(try!(args[1].desymbolize(env)).recip());

            Ok(Atom(try!((num * nearest + half).floor()) / nearest))
        },
        Floor => Ok(Atom(try!(num.floor()))),
        Ceiling => Ok(Atom(try!(num.ceil()))),
        Zero => Ok(Atom(Boolean(num == num::zero()))),
        _ => Ok(Atom(Boolean((op != Even) ^ 
                             (try!(num.numer()).is_even() && try!(num.is_integer())))))
    }
}
