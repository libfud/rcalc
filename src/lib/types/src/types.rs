#![crate_name = "types"]
#![crate_type = "lib"]
#![feature(core,collections)]

extern crate matrix;

pub use self::matrix::MatrixErrors;
pub use literal::{LiteralType, WithEnv};
pub use sexpr::ArgType;
pub use sexpr::ArgType::{Atom, SExpr};
pub use sexpr::Expression;
pub use operator::OperatorType;
use std::collections::HashMap;
use std::fmt;

pub mod sexpr;
pub mod literal;
pub mod operator;
pub mod record;

pub type CalcResult<T = ArgType> = Result<T, ErrorKind>;
pub type Env = Environment;
pub type Args = Vec<ArgType>;
pub type Expr = Expression;

#[derive(Clone, PartialEq)]
pub enum ErrorKind {
    BadExpr,
    BadToken(String),
    BadPowerRange,
    BadFloatRange,
    MatrixErr(MatrixErrors),
    BadNumberOfArgs(String, String, usize),
    BadArgType(String),
    UnexpectedVal(String, String),
    UnboundArg(String),
}

impl fmt::Display for ErrorKind {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            ErrorKind::BadArgType(ref x) => x.clone(),
            ErrorKind::BadExpr => "Malformed expression".to_string(),
            ErrorKind::BadToken(ref x) => x.clone(),
            ErrorKind::BadPowerRange => "Exponent too large for builtin `pow'!".to_string(),
            ErrorKind::BadFloatRange => "Number too large or precise for `exp' and `log'".to_string(),
            ErrorKind::MatrixErr(ref x) => x.to_string(),
            ErrorKind::BadNumberOfArgs(ref x, ref y, args) => format!("`{}' requires {} {} argument{}.", 
                        x, y, args, if args == 1 { "" } else { "" }),
            ErrorKind::UnexpectedVal(ref x, ref y) => format!("Expected {} but found {}", x, y),
            ErrorKind::UnboundArg(ref x) => format!("Error: Unbound variable `{}'", x),
        }));
        Ok(())
    }
}

#[derive(Clone)]
pub struct Environment {
    pub symbols: HashMap<String, LiteralType>,
    pub parent: Option<Box<Environment>>
}

impl<'a> Environment {
    pub fn new_global() -> Environment {
        Environment { symbols: HashMap::new(), parent: None }
    }

    pub fn new_frame(par: &Environment) -> Environment {
        Environment { symbols: HashMap::new(), parent: Some(Box::new(par.clone())) }
    }

    pub fn lookup(&'a self, var: &String) -> CalcResult<&'a LiteralType> {
        match self.symbols.get(var) {
            Some(val) => Ok(val),
            None      => match self.parent {
                Some(ref par) => par.lookup(var),
                None => Err(ErrorKind::UnboundArg(var.clone()))
            }
        }
    }
}

impl fmt::Display for Environment {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(fmt, "{:?}", self.symbols));
        try!(write!(fmt, "Has {} parent.", if self.parent.is_some() { "a" } else { "no" }));
        Ok(())
    }
}
