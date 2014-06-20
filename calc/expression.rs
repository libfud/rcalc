//! Expressions

extern crate num;

use std::num;
use self::num::rational::BigRational;

use super::{function, operator, CalcResult, Evaluate, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::operator::OperatorType;
use super::literal::LiteralType;

#[deriving(Show, Clone, PartialEq)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}


pub fn token_to_expr(token: Token) -> Result<ExprType, String> {
    match token {
        tokenize::Variable(x) => Ok(Function(x)),
        tokenize::Operator(op_ty) => Ok(Operator(op_ty)),
        _ => Err("Not a valid token!".to_str())
    }
}

#[deriving(Clone)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<Box<Evaluate>>) -> Expression {
        Expression { expr_type: e, args: a }
    }

    pub fn box_it(self) -> Box<Evaluate> {
        box self as Box<Evaluate>
    }
}

impl Evaluate for Expression {
    fn eval(&self, env: &mut Environment) -> CalcResult {
        match self.expr_type {
            Operator(op_type)   => {
                operator::eval(op_type, &self.args, env)
            }
            Function(ref fn_name)    => {
                function::eval(fn_name, &self.args, env)
            }
        }
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
        self.expr_type.to_str()
    }
}

#[deriving(Show, Clone, PartialEq)]
pub struct SExpression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl SExpression {
    fn new(e: ExprType, a: Vec<ArgType>) -> SExpression {
        SExpression { expr_type: e, args: a }
    }
}

pub trait OtherEval {
    fn eval(&self, mut env: &mut Environment) -> CalcResult<ArgType>;

    fn to_symbol(&self, env: &mut Environment) -> String;
}

impl OtherEval for SExpression {
    fn eval(&self, env: &mut Environment) -> CalcResult<ArgType> {
        match self.expr_type {
            Operator(super::operator::Add) => test_add(&self.args, env),
            _ => Err("Not yet defined".to_str())
        }
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
        self.expr_type.to_str()
    }
}

#[deriving(Clone, Show, PartialEq)]
pub enum ArgType {
    Atom(BasicType),
    SExpr(SExpression),
}

#[deriving(Clone, Show, PartialEq)]
pub enum BasicType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, SExpression),
    List(Vec<LiteralType>),
    Void
}

fn test_add(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult<ArgType> {
    let mut numbers: Vec<BigRational> = Vec::new();
    for arg in args.iter() {
        match arg {
            &SExpr(ref x) => match try!(x.eval(env)) {
                Atom(BigNum(y)) => numbers.push(y),
                _ => return Err("Whoops".to_str())
            },
            &Atom(BigNum(ref x)) => numbers.push(x.clone()),
            _ => return Err("Not covering that yet".to_str())
        }
    }

    for num in numbers.iter() {
        println!("{}", num);
    }

    let zero: BigRational = num::zero();
    Ok(Atom(BigNum(numbers.iter().fold(zero, |x, y| x + *y))))
}

//Just seeing if this might be a valid path to walk down.
pub fn test() {
    let mut top_frame = super::Environment::new_global();

    let new_e = SExpression::new(Operator(super::operator::Add),
                                 vec!(Atom(BigNum(from_str::<BigRational>("1/1").unwrap()))));

    let new_r = SExpression::new(Operator(super::operator::Add),
                                 vec!(SExpr(new_e), 
                                      Atom(BigNum(from_str::<BigRational>("2/1").unwrap()))));

    let answer = new_r.eval(&mut top_frame);
    let right = from_str::<BigRational>("3/1").unwrap();
    println!("correct: {}, answer: {}", right, answer);
}
