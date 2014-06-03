//! Operators

use std::num;
use super::{Evaluate, CalcResult, Environment, lookup, funfind, function};
use super::common::{rational_to_f64_trig, str_to_rational};
use super::literal::{LiteralType, Boolean, BigNum, Symbol, Func, Void};

pub mod power;
pub mod arithmetic;
pub mod logic;

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
    Rad,
    Deg,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    If,
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
        "rad"   => Some(Rad),
        "deg"   => Some(Deg),
        "<"     => Some(Lt),
        "<="    => Some(LtEq),
        "="     => Some(Eq),
        ">="    => Some(GtEq),
        ">"     => Some(Gt),
        "if"    => Some(If),
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
        Rad     => "rad",
        Deg     => "deg",
        Lt      => "<",
        LtEq    => "<=",
        Eq      => "=",
        GtEq    => ">=",
        Gt      => ">",
        If      => "if",
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

        Sin => {
            if args.len() > 1 {
                return Err("'sin' takes one argument".to_str())
            }

            let evaluated_array = try!(unbox_it(args, env));
            let evaluated = match evaluated_array.as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => {
                    return Err("I'm too tired to do this right now.".to_str())
                },
            };
            
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.sin().to_str();
            let answer = try!(str_to_rational(penult_answer.as_slice()));
            
            Ok(BigNum(answer))
        },

        Cos => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_str())
            }
            let evaluated = match (try!(unbox_it(args, env))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Something went wrong".to_str()) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.cos().to_str();
            let answer = try!(str_to_rational(penult_answer.as_slice()));
            
            Ok(BigNum(answer))
        },

        Tan => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_str())
            }
            let evaluated = match (try!(unbox_it(args, env))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Too tired".to_str()) }
            };

            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = (ration_as_float.sin() / ration_as_float.cos()).to_str();
            let answer = try!(str_to_rational(penult_answer.as_slice()));
            
            Ok(BigNum(answer))
        },

        Rad => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_str())
            }
            
            let degrees = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let radians = degrees.mul(&pi.div(&one80));

            Ok(radians) */
            Ok(Boolean(true))
        },

        Deg => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_str())
            }

            let radians = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let degrees = radians.mul(&one80.div(&pi));

            Ok(degrees)
            */
            Ok(Boolean(true))
        },

        If   => logic::cond(args, env),

        Lt   => logic::ordring(args, env, |a, b| a < b),

        LtEq => logic::ordering(args, env, |a, b| a <= b),

        Eq   => logic::ordering(args, env, |a, b| a == b),

        GtEq => logic::ordering(args, env, |a, b| a >= b),
        
        Gt   => logic::ordering(args, env, |a, b| a > b),
    }
}
