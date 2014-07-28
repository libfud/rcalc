//! Expressions

use super::{LiteralType, Environment, OperatorType};

#[deriving(Show, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum ExprType {
    BuiltIn(OperatorType),
    Function(String)
}

#[deriving(Clone, Show, PartialEq, PartialOrd, Eq, Ord)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<ArgType>) -> Expression {
        Expression { expr_type: e, args: a }
    }
    pub fn to_symbol(&self, env: &Environment) -> String {
        let mut symbols = String::new();
        symbols = symbols.append("(");
        match self.expr_type {
            Function(ref f) => {
                symbols = symbols.append(f.as_slice());
            }
            BuiltIn(ref op) => {
                symbols = symbols.append(op.to_string().append(" ").as_slice());
            }
        }
        for argument in self.args.iter() {
            let arg = match argument {
                &Atom(ref x) => x.to_string(),
                &SExpr(ref x) => x.to_symbol(env),
            };
            symbols = symbols.append(arg.append(" ").as_slice());
        }
        symbols = symbols.append(")");
        symbols
    }
}

#[deriving(Clone, Show, PartialEq, PartialOrd, Eq, Ord)]
pub enum ArgType {
    Atom(LiteralType),
    SExpr(Expression),
}
