//! The parent module to every other module in calc.

extern crate types;
extern crate parse;

pub use self::types::{CalcResult, Environment, ErrorKind, BadArgType, BadNumberOfArgs, 
                      BadPowerRange, BadFloatRange, Env, Args};
pub use self::types::sexpr::{Atom, SExpr, Expression, ArgType, BuiltIn, Function};
pub use self::types::literal::{Lit, LiteralType, Symbol, Proc, Void};
pub use self::common::help;

pub mod common;
pub mod function;
pub mod matrice;
pub mod operator;
pub mod record;

/// A structure to allow persistence of variables and functions

pub fn define(args: &Args, env: &mut Env) -> CalcResult {
    if args.len() < 2 {
        return Err(BadNumberOfArgs("define".to_string(), "only".to_string(), 2))
    }
    
    let name_and_vars = try!(try!(args[0].desymbolize(env)).to_vec());
    if name_and_vars.len() == 0 {
            return Err(BadArgType("Name required for definitions".to_string()))
    }

    let name = try!(name_and_vars[0].to_sym_string());
    let vars = if name_and_vars.len() == 1 {
        vec![]
    } else {
        let mut string_vec = Vec::new();
        for arg in name_and_vars.tail().iter() {
            string_vec.push(try!(arg.to_sym_string()));
        }
        string_vec
    };

    if args.len() == 2 {
        let arg = args.last().unwrap();
        match arg {
            &Atom(ref x) => { env.symbols.insert(name, x.clone()); },
            &SExpr(ref x) => match arg.eval(env) {
                Ok(Atom(y)) => { env.symbols.insert(name, y); },
                Ok(_) => fail!("Impossible!"),
                Err(_) => { env.symbols.insert(name, Proc(vars, x.clone())); }
            }
        }
        return Ok(Atom(Void))
    }

    //there's multiple expressions involved, so we just pack them all that way
    match args.last().unwrap() {
        &Atom(ref x)  => { env.symbols.insert(name, x.clone()); }
        &SExpr(ref x) => { 
            env.symbols.insert(name, Proc(vars, Expression::new(x.expr_type.clone(),
                                                                args.tail().to_vec())));
        }
    }
    Ok(Atom(Void))
}

pub trait Evaluate {
    fn eval(&self, env: &mut Environment) -> CalcResult;
    fn arg_to_literal(&self, env: &mut Environment) -> CalcResult<LiteralType>;
    fn desymbolize(&self, env: &mut Environment) -> CalcResult<LiteralType>;
}

impl Evaluate for ArgType {
    #[inline]
    fn eval(&self, env: &mut Environment) -> CalcResult {
        match self {
            &Atom(_) => Ok(self.clone()),
            &SExpr(ref s) =>  match s.expr_type {
                BuiltIn(x) => operator::eval(x, &s.args, env),
                Function(ref f) => function::eval(f, &s.args, env)
            }
        }
    }

    #[inline]
    fn arg_to_literal(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        match self {
            &Atom(ref x) => Ok(x.clone()),
            &SExpr(_) => try!(self.eval(env)).arg_to_literal(env)
        }
    }

    #[inline]
    fn desymbolize(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        match self {
            &Atom(Symbol(ref x)) => Atom(try!(env.lookup(x)).clone()).desymbolize(env),
            &Atom(ref x) => Ok(x.clone()),
            &SExpr(_) => try!(self.eval(env)).desymbolize(env)
        }
    }
}

impl Evaluate for Expression {
    #[inline]
    fn eval(&self, env: &mut Environment) -> CalcResult {
        match self.expr_type {
            BuiltIn(x) => operator::eval(x, &self.args, env),
            Function(ref f) => function::eval(f, &self.args, env)
        }
    }

    #[inline]
    fn arg_to_literal(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        let res = try!(self.eval(env)).arg_to_literal(env);
        res
    }

    #[inline]
    fn desymbolize(&self, env: &mut Environment) -> CalcResult<LiteralType> {
        let res = try!(self.eval(env)).desymbolize(env);
        res
    }
}

pub fn eval(s: &str, env: &mut Env) -> CalcResult {
    let expr = try!(self::parse::parse(s, env));

    expr.eval(env)
}
