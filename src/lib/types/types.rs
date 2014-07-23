#![crate_name = "types"]
#![crate_type = "lib"]
#![feature(default_type_params)]

extern crate matrix;

pub use self::matrix::{Matrice, MatrixErrors};
//pub use self::num::rational::{BigRational, Ratio};
pub use literal::{LiteralType};
pub use sexpr::{ArgType, Atom, SExpr, Expression};
pub use operator::OperatorType;
use std::collections::hashmap::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

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
    BadNumberOfArgs(String, String, uint),
    BadArgType(String),
    DivByZero,
    NonBoolean,
    UnexpectedVal(String, String),
    UnboundArg(String),
}

impl fmt::Show for ErrorKind {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let res = match self {
            &BadArgType(ref x) => x.clone(),
            &BadExpr => "Malformed expression".to_string(),
            &BadToken(ref x) => x.clone(),
            &BadPowerRange => "Exponent too large for builtin `pow'!".to_string(),
            &BadFloatRange => "Number too large or precise for `exp' and `log'".to_string(),
            &MatrixErr(ref x) => x.to_string(),
            &BadNumberOfArgs(ref x, ref y, args) => 
                format!("`{}' requires {} {} {}.", x, y, args, match args {
                    1 => "argument",
                    _ => "arguments"
                }),
            &DivByZero => "Attempted division by zero!".to_string(),
            &NonBoolean => "Non boolean condition".to_string(),
            &UnexpectedVal(ref x, ref y) => format!("Expected {} but found {}", x, y),
            &UnboundArg(ref x) => format!("Error: Unbound variable `{}'", x),
        };
        try!(write!(fmt, "{}", res));
        Ok(())
    }
}

pub type CalcResult<T = ArgType> = Result<T, ErrorKind>;

#[deriving(Clone)]
pub struct Environment {
    pub symbols: RefCell<HashMap<String, LiteralType>>,
    pub parent: Option<Rc<Environment>>
}

impl<'a> Environment {
    pub fn new_global() -> Rc<Environment> {
        Rc::new(Environment { 
            symbols: RefCell::new(HashMap::new()), parent: None })
    }

    pub fn new_frame(par: Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment { 
            symbols: RefCell::new(HashMap::new()), parent: Some(par) })
    }

    pub fn lookup(&'a self, var: &String) -> CalcResult<&'a LiteralType> {
        /*
        match self.symbols.borrow().find(var) {
            Some(val) => Ok(val),
            None      => match self.parent {
                Some(ref par) => par.lookup(var),
                None => Err(UnboundArg(var.clone()))
            }
        }
         */
        let maybe = self.symbols.borrow();
        match maybe.find(var) {
            Some(val) => Ok(val),
            None => match self.parent {
                Some(par) => par.lookup(var),
                None => Err(UnboundArg(var.clone()))
            }
        }
    }
}

impl fmt::Show for Environment {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(fmt, "{}", self.symbols.borrow()));
        try!(write!(fmt, "Has {} parent.", if self.parent.is_some() { "a" } else { "no" }));
        Ok(())
    }
}
