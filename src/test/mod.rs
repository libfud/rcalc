//! Testing module for all of rcalc.

extern crate types;

use std::num;
use self::types::{/*CalcResult, ErrorKind, */ Environment, BadNumberOfArgs};
use self::types::sexpr::{Atom, /*SExpr*/};
use self::types::literal::{LiteralType};

use super::eval;

#[test]
fn arith_test() {
    let zero: LiteralType = num::zero();
    let one: LiteralType = num::one();
    let two: LiteralType = one + one;

    let mut env = Environment::new_global();
    assert_eq!(eval("(+ 2 2)", &mut env), Ok(Atom(two + two)));
    assert_eq!(eval("(+)", &mut env), Ok(Atom(zero.clone())));
    assert_eq!(eval("(-)", &mut env), 
               Err(BadNumberOfArgs("-".to_string(), "at least".to_string(), 1)));
    assert_eq!(eval("(pow 4 1/2)", &mut env), Ok(Atom(two.clone())));
    assert_eq!(eval("(sin 0)", &mut env), Ok(Atom(zero.clone())));
}
