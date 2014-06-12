//! Operators

extern crate num;

use self::num::rational::{Ratio, BigRational};
use self::num::bigint::*;

use std::num;
use super::{Evaluate, CalcResult, Environment, lookup, funfind, function};
use super::literal::{LiteralType, Boolean, BigNum, Symbol, Func, Void, trans_literal};

pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod trig;

#[deriving(Show, Clone)]
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
    Defun,
    Sum,
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
        "defun" => Some(Defun),
        "sum"   => Some(Sum),
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
        Defun   => "defun",
        Sum     => "sum",
        Help    => "help",
    };

    answer.to_str()
}

pub fn unbox_it(args:&Vec<Box<Evaluate>>, env: &mut Environment) ->
                                                         CalcResult<Vec<LiteralType>> {
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

        Sum  => sum(args, env),

        Help => super::common::help(args, env),
    }
}

pub fn get_int(arg: LiteralType) -> Result<int, String> {
    let x = match arg {
        BigNum(num) => num.to_integer().to_int(),
        _  => return Err("Not a number!".to_str())
    };

    if x.is_some() {
        Ok(x.unwrap())
    } else {
        Err("Argument must be an integer!".to_str())
    }
}

pub fn sum(args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    if args.len() != 3 {
        return Err("Not enough args for summation".to_str())
    }

    match args.get(0).eval(env) {
        Ok(Symbol(_)) => { },
        _ => return Err("Not a function!".to_str())
    }

    let (a, b) = (try!(get_int(try!(args.get(1).eval(env)))), 
                  try!(get_int(try!(args.get(2).eval(env)))));

    let mut answer: BigRational = num::zero();
    for x in range(a, b + 1) {
        let temp = BigNum(Ratio::new(x.to_bigint().unwrap(), num::one()));
        let expr = super::expression::Expression::new_raw(try!(args.get(0).eval(env)),
                      vec!(try!(trans_literal(temp, env))));
        answer = answer + match try!(expr.eval(env)) {
            BigNum(x) => x,
            _ => return Err("Invalid return type!".to_str())
        };
    }

    Ok(BigNum(answer))
}
