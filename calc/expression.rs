
extern crate num;

use self::num::rational::BigRational;
use super::{CalcResult, Evaluate};
use super::function;
use super::function::FunctionType;
use super::operator;
use super::operator::OperatorType;

#[deriving(Show)]
pub enum ExprType {
    Operator(OperatorType),
    Function(FunctionType)
}

pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>
}

impl Evaluate for Expression {
    fn eval(&self) -> CalcResult {
        match self.expr_type {
            Operator(op_type)   => {
                operator::eval(op_type, &self.args)
            }
            Function(f_type)    => {
                function::eval(f_type, &self.args)
            }
        }
    }
}
