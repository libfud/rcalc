//! Expressions

use super::{CalcResult, Evaluate};
use super::{function, operator, Environment};
use super::operator::OperatorType;

#[deriving(Show)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}

pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>,
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
}
