//! Testing module for all of rcalc.

extern crate types;

use std::num;
use self::types::{/*CalcResult, ErrorKind, */ Environment};
use self::types::sexpr::{Atom, /*SExpr*/};
use self::types::literal::{LiteralType};

use super::eval;

#[test]
fn add_test() {
    let zero: LiteralType = num::zero();
    let one: LiteralType = num::one();
    let two: LiteralType = one + one;

    let mut env = Environment::new_global();
    assert_eq!(eval("(+ 2 2)", &mut env), Ok(Atom(two + two)));

    assert_eq!(eval("(+)", &mut env), Ok(Atom(zero.clone())));
}

    

