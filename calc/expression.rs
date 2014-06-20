//! Expressions

extern crate num;

use self::num::rational::BigRational;

use super::{function, operator, CalcResult, Evaluate, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::operator::OperatorType;
use super::literal::LiteralType;

#[deriving(Show, Clone)]
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

#[deriving(Show, Clone)]
pub struct SExpression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl SExpression {
    fn new(e: ExprType, a: Vec<ArgType>) -> SExpression {
        SExpression { expr_type: e, args: a }
    }
}

impl Evaluate for SExpression {
    fn eval(&self, _: &mut Environment) -> CalcResult {
        Ok(super::literal::Void)
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
        self.expr_type.to_str()
    }
}

#[deriving(Clone, Show)]
pub enum ArgType {
    Atom(BasicType),
    SExpr(SExpression),
}

#[deriving(Clone, Show)]
pub enum BasicType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, SExpression),
    List(Vec<LiteralType>),
    Void
}

//Just seeing if this might be a valid path to walk down.
fn test() {
    let new_e = SExpression::new(Operator(super::operator::Add),
                                 vec!(Atom(
                                     BigNum(from_str::<BigRational>("1/1").
                                                             unwrap()))));

    let new_r = SExpression::new(Operator(super::operator::Add),
                                 vec!(SExpr(new_e)));
}
