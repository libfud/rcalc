//! Expressions

use super::{LiteralType, function, operator, CalcResult, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::literal::Void;
use super::operator::OperatorType;

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

#[deriving(Clone, Show, PartialEq)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<ArgType>) -> Expression {
        Expression { expr_type: e, args: a }
    }
  
    pub fn eval(&self, env: &mut Environment) -> CalcResult {
        if self.expr_type == Operator(operator::If) {
            return operator::eval(operator::If , &self.args, env)
        }
        
        let mut data =  Vec::new();
        
        for arg in self.args.iter() {
            match arg {
                &Atom(ref x) => data.push(Atom(x.clone())),
                &SExpr(ref x) => data.push( match x.clone().expr_type {
                    Operator(op_type)   => try!(operator::eval(op_type, &x.args, env)),
                    Function(ref fn_name) => try!(function::eval(fn_name, &x.args, env)),
                }),
            }
        }

        match self.expr_type {
            Operator(op) => operator::eval(op, &data, env),
            Function(ref fn_name) => function::eval(fn_name, &data, env),
        }
    }
}

#[deriving(Clone, Show, PartialEq)]
pub enum ArgType {
    Atom(LiteralType),
    SExpr(Expression),
}

pub fn arg_to_literal(arg: &ArgType, env: &mut Environment) -> CalcResult<LiteralType> {
    match arg {
        &SExpr(ref x) => arg_to_literal(&try!(x.eval(env)), env),
        &Atom(ref x) => Ok(x.clone())
    }
}

 
