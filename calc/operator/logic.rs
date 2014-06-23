//!Logic and odering.

extern crate num;

use self::num::rational::BigRational;
use super::super::expression::{Expression, Operator, ExprType};
use super::super::{CalcResult, Environment};
use super::super::literal::{Boolean, Symbol, BigNum};
use super::{If, ArgType, Atom, SExpr, arg_to_literal, desymbolize};

type Args<T = Vec<ArgType>> = T;
type Env<T = Environment> = T;

pub fn cond(args: &Args, env: &mut Env)  -> CalcResult {
    let mut expr_type = Operator(If);
    let mut arguments = args.clone();

    loop {
        if expr_type != Operator(If) {
            return Expression::new(expr_type, arguments).eval(env)
        }
        
        if args.len() != 3 {
            return Err("`if` requires three arguments".to_str())
        }

        let condition = match arguments.get(0) {
            &Atom(Boolean(x))  => x,
            &SExpr(ref x)   => match try!(x.eval(env)) {
                Atom(Boolean(y))  => y,
                _   => return Err("Only boolean expressions can be a condtion!".to_str())
            },
            _   => return Err("Only boolean expressions can be a condition!".to_str())
        };
        
        if condition {
            match arguments.get(1).clone() {
                Atom(_) => return Ok(arguments.get(1).clone()),
                SExpr(x) => {
                    if x.expr_type == Operator(If) {
                        expr_type = x.expr_type.clone();
                        arguments = x.args.clone();
                    } else {
                        return x.eval(env)
                    }
                }
            }
        } else {
            match arguments.get(2).clone() {
                Atom(_) => return Ok(arguments.get(2).clone()),
                SExpr(ref x) => {
                    if x.expr_type == Operator(If) {
                        expr_type = x.expr_type.clone();
                        arguments = x.args.clone();
                    } else {
                        return x.eval(env)
                    }
                }
            }
        }
    }
}

type BR = BigRational;

/*
pub fn to_num(arg: LiteralType, env: &mut Environment) -> LiteralType 
*/
pub fn ordering(args: &Args, env: &mut Env, comp: |&BR,&BR| -> bool) -> CalcResult {

    if args.len() != 2 {
        return Err("Ordering requires two arguments".to_str())
    }
    let (a, b) = (try!(desymbolize(args.get(0), env)),
                  try!(desymbolize(args.get(1), env)));
    match (&a, &b) {
        (&BigNum(ref x), &BigNum(ref y)) => Ok(Atom(Boolean(comp(x, y)))),
        _ =>  Err(format!("Ordering only takes numbers! {} {}",
                          a, b))
    }
}

pub fn equality(args: &Args, env: &mut Env, equal: bool) -> CalcResult {
    if args.len() != 2 {
        return Err("Equality comparisons require two arguments".to_str())
    }

    let (a, b) = (try!(arg_to_literal(args.get(0), env)), 
                  try!(arg_to_literal(args.get(1), env)));

    if equal {
        Ok(Atom(Boolean(a == b)))
    } else {
        Ok(Atom(Boolean(a != b)))
    }
}       

pub fn and_or(args: &Args, env: &mut Env, short: bool) -> CalcResult {

    if short == true {
        for val in args.iter() {
            match try!(arg_to_literal(val, env)) {
                Boolean(true)   => return Ok(Atom(Boolean(short))),
                Boolean(false)  => { },
                _   => return Err("Non boolean conditon!".to_str())
            }
        }
        
        Ok(Atom(Boolean(false)))
    } else {
        for val in args.iter() {
            match try!(arg_to_literal(val,env)) {
                Boolean(true)   => { },
                Boolean(false)  => return Ok(Atom(Boolean(short))),
                _   => return Err("Non boolean condition!".to_str())
            }
        }

        Ok(Atom(Boolean(true)))
    }
}

pub fn not(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() != 1 {
        return Err("Not only takes one argument".to_str())
    }

    let val = match try!(arg_to_literal(args.get(0), env)) {
        Boolean(x)  => x,
        _   => return Err("Non boolean condition!".to_str())
    };

    Ok(Atom(Boolean(!val)))
}
