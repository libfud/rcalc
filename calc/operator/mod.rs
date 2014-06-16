//! Operators

use std::num;
use super::{Evaluate, CalcResult, Environment};
use super::literal::{LiteralType, Symbol};

pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod trig;

#[deriving(Show, Clone, PartialOrd, PartialEq)] 
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Sin,
    Cos,
    Tan,
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    If,
    And,
    Or,
    Not,
    Define,
    Help,
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+"     => Some(Add),
        "-"     => Some(Sub),
        "*"     => Some(Mul),
        "/"     => Some(Div),
        "%"     => Some(Rem),
        "pow"   => Some(Pow),
        "sin"   => Some(Sin),
        "cos"   => Some(Cos),
        "tan"   => Some(Tan),
        "<"     => Some(Lt),
        "<="    => Some(LtEq),
        "="     => Some(Eq),
        "!="    => Some(NEq),
        ">="    => Some(GtEq),
        ">"     => Some(Gt),
        "if"    => Some(If),
        "and"   => Some(And),
        "or"    => Some(Or),
        "not"   => Some(Not),
        "define"=> Some(Define),
        "help"  => Some(Help),
        _       => None
    }
}

pub fn to_str(op: &OperatorType) -> String {
    let answer = match *op {
        Add     => "+",
        Sub     => "-",
        Mul     => "*",
        Div     => "/",
        Rem     => "%",
        Pow     => "pow",
        Sin     => "sin",
        Cos     => "cos",
        Tan     => "tan",
        Lt      => "<",
        LtEq    => "<=",
        Eq      => "=",
        NEq     => "!",
        GtEq    => ">=",
        Gt      => ">",
        If      => "if",
        And     => "and",
        Or      => "or",
        Not     => "not",
        Define  => "define",
        Help    => "help",
    };

    answer.to_str()
}

pub fn unbox_it(args:&Vec<Box<Evaluate>>, 
                env: &mut Environment) -> CalcResult<Vec<LiteralType>> {

    let mut literal_vec: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        let val = try!(arg.eval(env));
        literal_vec.push( match val {
            Symbol(ref var) => try!(env.lookup(var)),
            _   => val
        });
    }

    Ok(literal_vec)
}

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>, 
            env: &mut Environment) -> CalcResult {
    match op_type {
        Define  => Ok(super::literal::Void),

        Add => arithmetic::do_op(args, env, 0, |a, b| a + *b, num::zero),

        Sub => arithmetic::do_op(args, env, 1, |a, b| a - *b, num::zero),

        Mul => arithmetic::do_op(args, env, 0, |a, b| a * *b, num::one),

        Div => arithmetic::divrem(args, env, |a, b| a / *b), 
        //division can fail with zeros

        Rem => arithmetic::divrem(args, env, |a, b| a % *b),

        Pow => power::pow_wrapper(args, env),

        Sin => trig::trig(args, env, |x| x.sin()),

        Cos => trig::trig(args, env, |x| x.cos()),

        Tan => trig::trig(args, env, |x| x.tan()),

        If   => logic::cond(args, env),

        And  => logic::and_or(args, env, false),

        Or   => logic::and_or(args, env, true),

        Not  => logic::not(args, env),

        Lt   => logic::ordering(args, env, |a, b| a < b),

        LtEq => logic::ordering(args, env, |a, b| a <= b),

        Eq   => logic::equality(args, env, true),

        NEq  => logic::equality(args, env, false),

        GtEq => logic::ordering(args, env, |a, b| a >= b),
        
        Gt   => logic::ordering(args, env, |a, b| a > b),

        Help => super::common::help(args),
    }
}
