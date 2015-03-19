#![crate_name = "types"]
#![crate_type = "lib"]
#![feature(default_type_params)]

extern crate matrix;

pub use self::matrix::MatrixErrors;
pub use literal::{LiteralType, WithEnv};
pub use sexpr::{ArgType, Atom, SExpr, Expression};
pub use operator::OperatorType;
use std::collections::hashmap::HashMap;
use std::fmt;

pub mod sexpr;
pub mod literal;
pub mod operator;
pub mod record;

pub type CalcResult<T = ArgType> = Result<T, ErrorKind>;
pub type Env = Environment;
pub type Args = Vec<ArgType>;
pub type Expr = Expression;

#[deriving(Clone, PartialEq)]
pub enum ErrorKind {
    BadExpr,
    BadToken(String),
    BadPowerRange,
    BadFloatRange,
    MatrixErr(MatrixErrors),
    BadNumberOfArgs(String, String, uint),
    BadArgType(String),
    UnexpectedVal(String, String),
    UnboundArg(String),
}

impl fmt::Show for ErrorKind {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            BadArgType(ref x) => x.clone(),
            BadExpr => "Malformed expression".to_string(),
            BadToken(ref x) => x.clone(),
            BadPowerRange => "Exponent too large for builtin `pow'!".to_string(),
            BadFloatRange => "Number too large or precise for `exp' and `log'".to_string(),
            MatrixErr(ref x) => x.to_string(),
            BadNumberOfArgs(ref x, ref y, args) => format!("`{}' requires {} {} argument{}.", 
                        x, y, args, if args == 1 { "" } else { "" }),
            UnexpectedVal(ref x, ref y) => format!("Expected {} but found {}", x, y),
            UnboundArg(ref x) => format!("Error: Unbound variable `{}'", x),
        }));
        Ok(())
    }
}

#[deriving(Clone)]
pub struct Environment {
    pub symbols: HashMap<String, LiteralType>,
    pub parent: Option<Box<Environment>>
}

impl<'a> Environment {
    pub fn new_global() -> Environment {
        Environment { symbols: HashMap::new(), parent: None }
    }

    pub fn new_frame(par: &Environment) -> Environment {
        Environment { symbols: HashMap::new(), parent: Some(box par.clone()) }
    }

    pub fn lookup(&'a self, var: &String) -> CalcResult<&'a LiteralType> {
        match self.symbols.find(var) {
            Some(val) => Ok(val),
            None      => match self.parent {
                Some(ref par) => par.lookup(var),
                None => Err(UnboundArg(var.clone()))
            }
        }
    }
}

impl fmt::Show for Environment {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(fmt, "{}", self.symbols));
        try!(write!(fmt, "Has {} parent.", if self.parent.is_some() { "a" } else { "no" }));
        Ok(())
    }
}
