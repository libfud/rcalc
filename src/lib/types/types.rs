#![crate_name = "types"]
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
use std::fmt;

pub mod sexpr;
pub mod literal;
pub mod operator;

#[deriving(Clone, PartialEq)]
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
            BadExpr => "Malformed expression".to_string(),
            BadToken(x) => x.clone(),
            BadPowerRange => "Exponent too large for builtin `pow'!".to_string(),
            BadFloatRange => "Number too large or precise for `exp' and `log'".to_string(),
            MatrixErr(x) => x.to_string(),
            BadNumberOfArgs(x) => x.clone(),
            DivByZero => "Attempted division by zero!".to_string(),
            NonBoolean => "Non boolean condition".to_string(),
            UnboundArg(x) => format!("Error: Unbound variable `{}'", x),
        }
    }
}

impl fmt::Show for ErrorKind {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            &BadArgType(ref x) => x.clone(),
            &BadExpr => "Malformed expression".to_string(),
            &BadToken(ref x) => x.clone(),
            &BadPowerRange => "Exponent too large for builtin `pow'!".to_string(),
            &BadFloatRange => "Number too large or precise for `exp' and `log'".to_string(),
            &MatrixErr(ref x) => x.to_string(),
            &BadNumberOfArgs(ref x) => x.clone(),
            &DivByZero => "Attempted division by zero!".to_string(),
            &NonBoolean => "Non boolean condition".to_string(),
            &UnboundArg(ref x) => format!("Error: Unbound variable `{}'", x),
        };
        print!("{}", res);
        Ok(())
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

impl fmt::Show for Environment {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        println!("{}", self.symbols);
        print!("Has {} parent.", if self.parent.is_some() { "a" } else { "no" });
        Ok(())
    }
}
