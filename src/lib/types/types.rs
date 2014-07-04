#![crate_id = "types"]
#![crate_type = "lib"]
#![feature(default_type_params)]

extern crate num;
extern crate matrix;

pub use self::matrix::{Matrice, MatrixErrors};
pub use self::num::rational::{BigRational, Ratio};
pub use self::num::bigint;
pub use literal::{LiteralType};
pub use sexpr::{ArgType, Atom, SExpr, Expression};
pub use operator::OperatorType;
use std::collections::hashmap::HashMap;

pub mod sexpr;
pub mod literal;
pub mod operator;

#[deriving(Clone, Show)]
pub enum ErrorKind {
    BadExpr,
    BadToken(String),
    BadPowerRange,
    BadFloatRange,
    MatrixErr(MatrixErrors),
    BadNumberOfArgs(String),
    BadArgType(String),
    DivByZero,
    NonBoolean,
    UnboundArg(String),
}

impl ErrorKind {
    pub fn to_symbol(self) -> String {
        match self {
            BadArgType(x) => x.clone(),
            BadExpr => "Malformed expression".to_str(),
            BadToken(x) => x.clone(),
            BadPowerRange => "Exponent too large for builtin `pow'!".to_str(),
            BadFloatRange => "Number too large or precise for `exp' and `log'".to_str(),
            MatrixErr(x) => x.to_str(),
            BadNumberOfArgs(x) => x.clone(),
            DivByZero => "Attempted division by zero!".to_str(),
            NonBoolean => "Non boolean condition".to_str(),
            UnboundArg(x) => format!("Error: Unbound variable `{}'", x),
        }
    }
}

pub type CalcResult<T = ArgType> = Result<T, ErrorKind>;

#[deriving(Clone)]
pub struct Environment {
    pub symbols: HashMap<String, LiteralType>,
    pub parent: Option<Box<Environment>>
}

impl Environment {
    pub fn new_global() -> Environment {
        Environment { symbols:  HashMap::new(), parent: None }
    }

    pub fn new_frame(par: &mut Environment) -> Environment {
        Environment { symbols: HashMap::new(), parent: Some(box par.clone()) }
    }

    pub fn lookup(&self, var: &String) -> CalcResult<LiteralType> {
        match self.symbols.find(var) {
            Some(val) => Ok(val.clone()),
            None      => {
                if self.parent.is_some() {
                    self.parent.clone().unwrap().lookup(var)
                } else {
                    Err(UnboundArg(var.clone()))
                }
            }
        }
    }
}
