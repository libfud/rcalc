//! Expressions

use super::{LiteralType, function, operator, CalcResult, Environment, BadToken};
use super::tokenize;
use super::tokenize::Token;
use super::operator::OperatorType;
use super::pretty::pretty;

#[deriving(Show, Clone, PartialEq, PartialOrd)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}

#[deriving(Clone, Show, PartialEq, PartialOrd)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<ArgType>) -> Expression {
        Expression { expr_type: e, args: a }
    }
    pub fn eval(&self, env: &mut Environment) -> CalcResult {
        match self.expr_type {
            Operator(op_type)   => {
                operator::eval(op_type, &self.args, env)
            }
            Function(ref fn_name)    => {
                function::eval(fn_name, &self.args, env)
            }
        }
    }
    pub fn to_symbol(&self, env: &mut Environment) -> String {
        let mut symbols = String::new();
        symbols = symbols.append("(");
        match self.expr_type {
            Function(ref f) => {
                symbols = symbols.append(f.as_slice());
            }
            Operator(ref op) => {
                symbols = symbols.append(super::operator::to_str(op).append(" ").as_slice());
            }
        }
        for argument in self.args.iter() {
            let arg = match argument {
                &Atom(ref x) => pretty(x, env).to_str(),
                &SExpr(ref x) => x.to_symbol(env),
            };
            symbols = symbols.append(arg.as_slice());
        }
        symbols = symbols.append(")");
        symbols
    }
}

#[deriving(Clone, Show, PartialEq, PartialOrd)]
pub enum ArgType {
    Atom(LiteralType),
    SExpr(Expression),
}

impl ArgType {
    pub fn arg_to_literal(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        match self {
            &SExpr(ref x) => try!(x.eval(env)).arg_to_literal(env),
            &Atom(ref x) => Ok(x.clone())
        }
    }

    pub fn desymbolize(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        match try!(self.arg_to_literal(env)) {
            super::literal::Symbol(x) => env.lookup(&x),
            otherwise => Ok(otherwise)
        }
    }
}
