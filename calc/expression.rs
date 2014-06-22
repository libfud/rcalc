//! Expressions

use super::{LiteralType, function, operator, CalcResult, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::literal::Boolean;
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
        let mut e_type = self.expr_type.clone();
        let mut arguments = self.args.clone();

        loop {
            if e_type == Operator(operator::If) {
                if arguments.len() != 3 {
                    return Err("`if` requires three arguments".to_str())
                }
            
                let condition = match arguments.get(0) {
                    &Atom(ref x) => match x {
                        &Boolean(val) => val,
                        _   => return Err("Only boolean expressions can be a condition!".to_str()),
                    },
                    &SExpr(ref x) => match try!(x.eval(env)) {
                        Atom(Boolean(val)) => val,
                        _ => return Err("Only booleans can be conditions!".to_str())
                    }
                };

                if condition {
                    match arguments.get(1).clone() {
                        Atom(_) => return Ok(arguments.get(1).clone()),
                        SExpr(x) => {
                            e_type = x.expr_type.clone();
                            arguments = x.args.clone();
                        }
                    }
                } else {
                    match arguments.get(2).clone() {
                        Atom(_) => return Ok(arguments.get(2).clone()),
                        SExpr(ref x) => {
                            e_type = x.expr_type.clone();
                            arguments = x.args.clone();
                        }
                    }
                }
            } else {
                break
            }
        }

        let mut data =  Vec::new();
        
        for arg in self.args.iter() {
            match arg {
                &Atom(ref x) => data.push(Atom(x.clone())),
                &SExpr(ref x) => data.push( match x.clone().expr_type {
                    Operator(op_type)   => try!(operator::eval(op_type, &x.args, env)),
                    Function(ref fn_name) => try!(function::eval(fn_name, &x.args, env))
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

 
