//! Logic and odering.

extern crate types;

use super::{zero, one};
use self::types::sexpr::ExprType::BuiltIn;
use self::types::literal::LiteralType::Boolean;
use self::types::literal::Lit;
use self::types::operator::*;
use self::types::operator::Gate::*;
use self::types::operator::RoundId::*;
use self::types::operator::Introspect::*;
use self::types::operator::OperatorType::*;
use self::types::ArgType::{Atom, SExpr};
use self::types::ErrorKind::BadNumberOfArgs;
use super::super::{Args, Evaluate, CalcResult, Env};

#[inline]   
pub fn handle_logic(args: &Args, env: &mut Env, log: Gate) -> CalcResult {
    match log {
        If  => cond(args, env),
        And => and_or(args, env, false), 
        Or  => and_or(args, env, true),
        Not => not(args, env), 
        Xor => xor(args, env),
    }
}

#[inline]
pub fn query(args: &Args, env: &mut Env, query: Introspect) -> CalcResult {
    if args.len() != 1 {
        return Err(BadNumberOfArgs(query.to_string(), "only".to_string(), 1))
    }

    let arg = try!(args[0].desymbolize(env));
    match query {
        BoolQ => Ok(Atom(Boolean(arg.is_bool()))),
        LambdaQ => Ok(Atom(Boolean(arg.is_proc()))),
        ListQ => Ok(Atom(Boolean(arg.is_list()))),
        MatrixQ => Ok(Atom(Boolean(arg.is_matrix()))),
        NumberQ => Ok(Atom(Boolean(arg.is_num()))),
        StructQ => Ok(Atom(Boolean(arg.is_structure()))),
        SymbolQ => Ok(Atom(Boolean(arg.is_symbol()))),
    }
}

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
        if try!(try!(val.desymbolize(env)).to_bool()) == short {
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
    let one: Lit = one();
    let two = one.clone() + one.clone();

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
            Ok(Atom(try!((num * nearest.clone() + (one / two)).floor()) / nearest))
        },
        Floor => Ok(Atom(try!(num.floor()))),
        Ceiling => Ok(Atom(try!(num.ceil()))),
        Zero => Ok(Atom(Boolean(num == zero()))),
        _ => Ok(Atom(Boolean((op != Even) ^ (try!(num.is_even())))))
    }
}
