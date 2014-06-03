//!Logic and odering.

use super::super::{Evaluate, CalcResult, Environment, lookup};
use super::super::literal::{LiteralType, Boolean, Symbol};
use super::unbox_it;

pub enum Gates {
    Nand,
    And,
    Not,
    Or,
    Nor,
    Xor,
    XNor
}

pub fn cond(args: &Vec<Box<Evaluate>>, env: &mut Environment)  -> CalcResult {
    if args.len() != 3 {
        return Err("`if` requires three arguments".to_str())
    }

    let condition = match try!(args.get(0).eval(env)) {
        Boolean(x)  => x,
        Symbol(x)   => {
            match try!(lookup(&x, env)) {
                Boolean(y)  => y,
                _   => return Err("Only boolean expressions can be a condtion!".to_str())
            }
        },
        _   => return Err("Only boolean expressions can be a condition!".to_str())
    };

    if condition {
        Ok(try!(args.get(1).eval(env)))
    } else {
        Ok(try!(args.get(2).eval(env)))
    }
}

pub fn ordering(args: &Vec<Box<Evaluate>>, env: &mut Environment, 
                        comp: |&LiteralType, &LiteralType| -> bool) -> CalcResult {

    if args.len() != 2 {
        return Err("Ordering requires two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));
    Ok(Boolean(comp(comparands.get(0), comparands.get(1))))
}

pub fn equality(args: &Vec<Box<Evaluate>>, env: &mut Environment, equal: bool) -> CalcResult {
    if args.len() != 2 {
        return Err("Equality comparisons require two arguments".to_str())
    }

    let comparands = try!(unbox_it(args, env));

    if equal {
        Ok(Boolean(comparands.get(0) == comparands.get(1)))
    } else {
        Ok(Boolean(comparands.get(0) != comparands.get(1)))
    }
}

pub fn and_or(args: &Vec<Box<Evaluate>>, env: &mut Environment, short: bool) -> CalcResult {
    let vals = try!(unbox_it(args, env));

    if short == true {
        for val in vals.iter() {
            match *val {
                Boolean(true)   => return Ok(Boolean(short)),
                Boolean(false)  => { },
                _   => return Err("Non boolean conditon!".to_str())
            }
        }
        
        Ok(Boolean(false))
    } else {
        for val in vals.iter() {
            match *val {
                Boolean(true)   => { },
                Boolean(false)  => return Ok(Boolean(short)),
                _   => return Err("Non boolean condition!".to_str())
            }
        }

        Ok(Boolean(true))
    }
}

pub fn not(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 1 {
        return Err("Not only takes one argument".to_str())
    }

    let val = match try!(unbox_it(args, env)).get(0) {
        &Boolean(x)  => x,
        _   => return Err("Non boolean condition!".to_str())
    };

    Ok(Boolean(!val))
}
