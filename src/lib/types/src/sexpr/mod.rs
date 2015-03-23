//! Expressions

use std::fmt;

use super::{LiteralType, Environment, OperatorType};

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum ExprType {
    BuiltIn(OperatorType),
    Function(String)
}

impl fmt::Display for ExprType {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            ExprType::BuiltIn(ref op) => op.to_string(),
            ExprType::Function(ref s) => s.clone(),
        }));
        Ok(())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl fmt::Display for Expression {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "({} ", self.expr_type));
        for arg in self.args.init().iter() {
            try!(write!(fmt, "{} ", arg));
        }
        try!(write!(fmt, "{})", match self.args.last() {
            Some(x) => x.to_string(),
            None => "".to_string()
        }));
        Ok(())
    }
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<ArgType>) -> Expression {
        Expression { expr_type: e, args: a }
    }
    pub fn to_symbol(&self, env: &Environment) -> String {
        let mut symbols = String::new();
        symbols.push_str("(");
        match self.expr_type {
            ExprType::Function(ref f) => {
                symbols.push_str(f.as_slice());
            }
            ExprType::BuiltIn(ref op) => {
                //FIXME
                let mut intermediate = op.to_string();
                intermediate.push_str(" ");
                symbols.push_str(intermediate.as_slice());
            }
        }
        for argument in self.args.iter() {
            let mut arg = match argument {
                &ArgType::Atom(ref x) => x.to_string(),
                &ArgType::SExpr(ref x) => x.to_symbol(env),
            };
            arg.push_str(" ");
            symbols.push_str(arg.as_slice());
        }
        symbols.push_str(")");
        symbols
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum ArgType {
    Atom(LiteralType),
    SExpr(Expression),
}

impl fmt::Display for ArgType {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &ArgType::Atom(ref x)  => x.to_string(),
            &ArgType::SExpr(ref x) => x.to_string()
        }));
        Ok(())
    }
}
