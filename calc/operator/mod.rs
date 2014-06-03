//! Operators

use std::num;
use super::{Evaluate, CalcResult, Environment, lookup, funfind, function};
use super::literal::{LiteralType, Boolean, BigNum, Symbol, Func, Void};

pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod trig;

#[deriving(Show)]
#[deriving(Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
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
    Defun,
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+"     => Some(Add),
        "-"     => Some(Sub),
        "*"     => Some(Mul),
        "/"     => Some(Div),
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
        "defun" => Some(Defun),
        _       => None
    }
}

pub fn to_str(op: &OperatorType) -> String {
    let answer = match *op {
        Add     => "+",
        Sub     => "-",
        Mul     => "*",
        Div     => "/",
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
        Defun   => "defun",
    };

    answer.to_str()
}

pub fn unbox_it(args:&Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult<Vec<LiteralType>> {
    let mut literal_vec: Vec<LiteralType> = Vec::new();
    for arg in args.iter() {
        let val = try!(arg.eval(env));
        literal_vec.push( match val {
            Symbol(ref var) => try!(lookup(var, env)),
            Func(ref var)   => {
                let (_, fun) = try!(super::funfind(var, env));
                Func(fun)
            },
            _   => val
        });
    }

    Ok(literal_vec)
}

pub fn has_bigs_or_bools(args: &Vec<LiteralType>) -> (bool, bool) {
    let mut bignum_flag = false;
    let mut bool_flag = false; //lol

    for literal in args.iter() {
        match literal {
            &BigNum(_)  => bignum_flag = true,
            &Boolean(_) => bool_flag = true,
            _   => { } //do nothing
        }
    }

    (bignum_flag, bool_flag)
}

pub fn define(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 2 {
        return Err("Define must have one symbol and one expression!".to_str())
    }

    let var = match try!(args.get(0).eval(env)) {
        Symbol(x)   => x,
        _   => {
            return Err("Attempted illegal definition!".to_str())
        }
    };

    let val = try!(args.get(1).eval(env));
    
    env.vars.insert(var, val);
    Ok(Void)
}

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    match op_type {
        Define  => define(args, env),

        Defun   => function::defun(args, env),

        Add => arithmetic::do_op(args, env, 0, |a, b| a + *b, num::zero),

        Sub => arithmetic::do_op(args, env, 1, |a, b| a - *b, num::zero),

        Mul => arithmetic::do_op(args, env, 0, |a, b| a * *b, num::one),

        Div => arithmetic::div(args, env), //division can fail with zeros

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
    }
}
