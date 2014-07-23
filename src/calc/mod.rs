//! The parent module to every other module in calc.

extern crate num;
extern crate types;
extern crate parse;

pub use std::rc::Rc;
pub use std::cell::RefCell;
pub use self::num::rational::{BigRational, Ratio};
pub use self::num::bigint;
pub use self::types::{CalcResult, Environment, 
                      ErrorKind, BadArgType, BadNumberOfArgs, 
                      BadPowerRange, BadFloatRange, NonBoolean,
                      };
pub use self::types::sexpr::{Atom, SExpr, Expression, ArgType, BuiltIn, Function};
pub use self::types::literal::{Lit, LitRes, LiteralType, 
                               BigNum, Boolean, List, Matrix, Proc, Symbol, Void};
pub use self::common::help;
pub use self::literal::{cons, car, cdr, list};

pub mod matrice;
pub mod literal;
pub mod operator;
pub mod function;
pub mod common;
pub mod pretty;

/// A structure to allow persistence of variables and functions

pub fn define(args: &Vec<ArgType>, env: &mut Rc<Environment>) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("define".to_string(), "only".to_string(), 2))
    }
    
    let name_and_vars = match try!(args[0].desymbolize(env)) {
        List(ref x) => if x.len() == 0 {
            return Err(BadArgType("Name required for definitions".to_string()))
        } else {
            x.clone()
        },
        x => return Err(BadArgType(format!("{} is not a symbol", x)))
    };

    let name = match name_and_vars[0] {
        Symbol(ref x) => x.clone(),
        _ => return Err(BadArgType("Names can only be symbols!".to_string()))
    };

    let vars = if name_and_vars.len() == 1 {
        vec![]
    } else {
        let mut string_vec = Vec::new();
        for arg in name_and_vars.tail().iter() {
            match arg {
                &Symbol(ref x) => string_vec.push(x.clone()),
                _ => return Err(BadArgType("Variables can only be symbols".to_string()))
            }
        }
        string_vec
    };

    if args.len() == 2 {
        let arg = args.last().unwrap();
        match arg {
            &Atom(ref x) => {
                env.symbols.insert(name, x.clone());
                return Ok(Atom(Void))
            }
            &SExpr(ref x) => {
                match arg.eval(env) {
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
            let expr = Expression::new(x.expr_type.clone(), args.tail().to_vec());
            env.symbols.insert(name, Proc(vars, expr));
            Ok(Atom(Void))
        }
    }
}

pub trait Evaluate {
    fn eval(&self, env: &mut Rc<Environment>) -> CalcResult;
    fn arg_to_literal(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType>;
    fn desymbolize(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType>;
}

impl Evaluate for ArgType {
    #[inline]
    fn eval(&self, env: &mut Rc<Environment>) -> CalcResult {
        match self {
            &Atom(_) => Ok(self.clone()),
            &SExpr(ref s) =>  match s.expr_type {
                BuiltIn(x) => operator::eval(x, &s.args, env),
                Function(ref f) => function::eval(f, &s.args, env)
            }
        }
    }

    #[inline]
    fn arg_to_literal(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType> {
        match self {
            &Atom(ref x) => Ok(x.clone()),
            &SExpr(_) => try!(self.eval(env)).arg_to_literal(env)
        }
    }

    #[inline]
    fn desymbolize(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType> {
        match self {
            &Atom(Symbol(ref x)) => Atom(try!(env.lookup(x)).clone()).desymbolize(env),
            &Atom(ref x) => Ok(x.clone()),
            &SExpr(_) => try!(self.eval(env)).desymbolize(env)
        }
    }
}

impl Evaluate for Expression {
    #[inline]
    fn eval(&self, env: &mut Rc<Environment>) -> CalcResult {
        match self.expr_type {
            BuiltIn(x) => operator::eval(x, &self.args, env),
            Function(ref f) => function::eval(f, &self.args, env)
        }
    }

    #[inline]
    fn arg_to_literal(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType> {
        let res = try!(self.eval(env)).arg_to_literal(env);
        res
    }

    #[inline]
    fn desymbolize(&self, env: &mut Rc<Environment>) -> CalcResult<LiteralType> {
        let res = try!(self.eval(env)).desymbolize(env);
        res
    }
}

/// Evaluates a string by creating a stream of tokens, translating those tokens
/// recursively, and then evaluating the top expression.
pub fn eval(s: &str, env: &mut Rc<Environment>) -> CalcResult {
    let expr = try!(self::parse::parse(s, env));

    expr.eval(env)
}
