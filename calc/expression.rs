//! Expressions

use super::{CalcResult, Evaluate};
use super::{function, operator, Environment};
use super::operator::OperatorType;

#[deriving(Show, Clone)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}

pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>,
}

impl Clone for Expression {
    fn clone(&self) -> Expression {
        Expression {
            expr_type: self.expr_type.clone(),
            args: self.args.clone()
        }
    }
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

    fn to_symbol(&self) -> String {
        self.expr_type.to_str()
    }
}
