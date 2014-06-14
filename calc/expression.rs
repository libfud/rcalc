//! Expressions

use super::{CalcResult, Evaluate};
use super::{function, operator, Environment};
use super::operator::OperatorType;
use super::literal::{LiteralType, Symbol};

#[deriving(Show)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}

pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<Box<Evaluate>>) -> Expression {
        Expression { expr_type: e, args: a }
    }

    pub fn new_raw(e: LiteralType, a: Vec<Box<Evaluate>>) -> Expression {
        match e {
            Symbol(x) => Expression::new(Function(x), a),
            _ => fail!("You used new_raw improperly!")
        }
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
}
