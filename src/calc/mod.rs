//! The parent module to every other module in calc.

extern crate num;

pub use self::num::rational::{BigRational, Ratio};
pub use self::num::bigint;
pub use self::expression::{Atom, SExpr, ArgType};
pub use self::literal::LiteralType;
pub use self::tokenize::{TokenStream, Token};
pub use self::common::help;
pub use std::collections::HashMap;

pub mod literal;
pub mod tokenize;
pub mod translate;
pub mod expression;
pub mod operator;
pub mod function;
pub mod common;
pub mod pretty;

/// A shortcut for the result type that is used everywhere
pub type CalcResult<T = ArgType> = Result<T, ErrorKind>;

#[deriving(Clone, Show)]
pub enum ErrorKind {
    BadExpr,
    BadToken(String),
    BadPowerRange,
    BadFloatRange,
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
            BadNumberOfArgs(x) => x.clone(),
            DivByZero => "Attempted division by zero!".to_str(),
            NonBoolean => "Non boolean condition".to_str(),
            UnboundArg(x) => format!("Error: Unbound variable `{}'", x),
        }
    }
}

/// A structure to allow persistence of variables and functions
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

    pub fn bind(vars: Vec<String>, vals: &Vec<ArgType>, 
                env: &mut Environment) -> CalcResult<Environment> {
        if vars.len() != vals.len() {
            return Err(BadNumberOfArgs("Arguments don't match number of variables".to_str()))
        }
        
        let mut child_env = Environment::new_frame(env);
        for (var, val) in vars.iter().zip(vals.iter()) {
            child_env.symbols.insert(var.clone(), try!(val.desymbolize(env)));
        }
        
        Ok(child_env)
    }
}

pub fn define(args: &Vec<ArgType>, env: &mut Environment) -> CalcResult {
    use self::expression::Expression;
    use self::literal::{List, Proc, Void, Symbol};
    if args.len() < 2 {
        return Err(BadNumberOfArgs(
            format!("Cannot define a variable without having a name and val")))
    }
    
    let name_and_vars = match try!(args.get(0).desymbolize(env)) {
        List(ref x) => if x.len() == 0 {
            return Err(BadArgType("Name required for definitions".to_str()))
        } else {
            x.clone()
        },
        x => return Err(BadArgType(format!("{} is not a symbol", x)))
    };

    let name = match name_and_vars.get(0) {
        &Symbol(ref x) => x.clone(),
        _ => return Err(BadArgType("Names can only be symbols!".to_str()))
    };

    let vars = if name_and_vars.len() == 1 {
        vec![]
    } else {
        let mut string_vec = Vec::new();
        for arg in name_and_vars.tail().iter() {
            match arg {
                &Symbol(ref x) => string_vec.push(x.clone()),
                _ => return Err(BadArgType("Variables can only be symbols".to_str()))
            }
        }
        string_vec
    };

    if args.len() == 2 {
        match args.last().unwrap() {
            &Atom(ref x) => {
                env.symbols.insert(name, x.clone());
                return Ok(Atom(Void))
            }
            &SExpr(ref x) => {
                match x.eval(env) {
                    Ok(res) => { 
                        env.symbols.insert(name, match res {
                            Atom(y) => y,
                            _ => fail!("Impossible!")
                        });
                    }
                    Err(_) => { env.symbols.insert(name, Proc(vars, x.clone())); }
                }
                return Ok(Atom(Void))
            }
        }
    }

    //there's multiple expressions involved, so we just pack them all that way
    match args.last().unwrap() {
        &Atom(ref x) => {
            env.symbols.insert(name, x.clone());
            Ok(Atom(Void))
        }
        &SExpr(ref x) => {
            let expr = Expression::new(x.expr_type.clone(), args.tail().to_owned());
            env.symbols.insert(name, Proc(vars, expr));
            Ok(Atom(Void))
        }
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Environment) -> CalcResult {
    use self::translate::top_translate;

    let mut tokens = TokenStream::new(s.to_str());

    let expr = try!(top_translate(&mut tokens, env));
    match expr {
        Atom(_) => Ok(expr),
        SExpr(x) => x.eval(env)
    }
}
