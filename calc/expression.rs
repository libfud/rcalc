//! Expressions

use super::{LiteralType, function, operator, CalcResult, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::operator::OperatorType;
use super::literal::{Proc, Symbol, Boolean};

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
        let mut ops_stack: Vec<ExprType> = Vec::new();
        let mut data: Vec<Vec<ArgType>> = vec!(self.args.clone());

        let mut holding_block: Vec<Vec<ArgType>> = Vec::new();
        loop {
            let arguments = data.pop().unwrap();
            data.push(vec![]);

            let mut arg_index = 0;
            while arg_index < arguments.len() {
                match arguments.as_slice()[arg_index].clone() {
                    Atom(_) => data.mut_last().unwrap().push(arguments.get(arg_index).clone()),
                    SExpr(x) => {
                        ops_stack.push(x.expr_type);
                        data.push(x.args);
                        if arguments.len() + 1 - arg_index > 0 {
                            holding_block.push(arguments.slice_from(arg_index + 1).to_owned());
                        } else {
                            holding_block.push(vec![])
                        }
                        break
                    }
                }
                arg_index += 1;
            }
            
            if ops_stack.len() > 0 {
                if data.last().unwrap().iter().all(|x| match x {
                    &Atom(_) => true,
                    &SExpr(_) => false,
                }) {
                    let op_args = data.pop().unwrap();
                    let result = match ops_stack.pop().unwrap() {
                        Operator(op) => try!(operator::eval(op, &op_args, env)),
                        Function(ref fn_name) => try!(function::eval(fn_name, &op_args, env)),
                    };
                    data.mut_last().unwrap().push(result);
                    data.mut_last().unwrap().push_all(holding_block.pop().unwrap().as_slice());
                }
            }

            if ops_stack.len() == 0 {
                break
            }                             
        }
        
        match self.expr_type {
            Operator(op) => operator::eval(op, data.get(0), env),
            Function(ref fn_name) => function::eval(fn_name, data.get(0), env),
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
