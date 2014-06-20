//! Expressions

extern crate num;

use std::num;
use self::num::rational::BigRational;

use super::{function, operator, CalcResult, Evaluate, Environment};
use super::tokenize;
use super::tokenize::Token;
use super::operator::OperatorType;
use super::literal::LiteralType;

use std::collections::hashmap::HashMap;

#[deriving(Show, Clone, PartialEq)]
pub enum ExprType {
    Operator(OperatorType),
    Function(String)
}

pub fn token_to_expr(token: Token) -> Result<ExprType, String> {
    match token {
        tokenize::Variable(x) => Ok(Function(x)),
        tokenize::Operator(op_ty) => Ok(Operator(op_ty)),
        _ => Err("Not a valid token!".to_str())
    }
}

#[deriving(Clone)]
pub struct Expression {
    pub expr_type: ExprType,
    pub args: Vec<Box<Evaluate>>,
}

impl Expression {
    pub fn new(e: ExprType, a: Vec<Box<Evaluate>>) -> Expression {
        Expression { expr_type: e, args: a }
    }

    pub fn box_it(self) -> Box<Evaluate> {
        box self as Box<Evaluate>
    }
}

impl Evaluate for Expression {
    fn eval(&self, env: &mut Environment) -> CalcResult {
        match self.expr_type {
            Operator(op_type)   => {
                operator::eval(op_type, &self.args, env)
            }
            Function(ref fn_name)    => {
                function::eval(fn_name, &self.args, env)
            }
        }
    }

    fn to_symbol(&self, _: &mut Environment) -> String {
        self.expr_type.to_str()
    }
}

/// A structure to allow persistence of variables and functions
#[deriving(Clone)]
pub struct Frame {
    pub symbols: HashMap<String, BasicType>,
    pub parent: Option<Box<Frame>>
}

impl Frame {
    pub fn new_global() -> Frame {
        Frame { symbols:  HashMap::new(), parent: None }
    }

    pub fn new_frame(par: &mut Frame) -> Frame {
        Frame { symbols: HashMap::new(), parent: Some(box par.clone()) }
    }

    pub fn lookup(&self, var: &String) -> CalcResult<BasicType> {
        match self.symbols.find(var) {
            Some(val) => Ok(val.clone()),
            None      => {
                if self.parent.is_some() {
                    match self.parent.clone().unwrap().lookup(var) {
                        Ok(v) => Ok(v.clone()),
                        Err(m) => Err(m)
                     }
                } else {
                    Err(format!("Unbound variable {}", var))
                }
            }
        }
    }
}

#[deriving(Show, Clone, PartialEq)]
pub struct SExpression {
    pub expr_type: ExprType,
    pub args: Vec<ArgType>,
}

impl SExpression {
    fn new(e: ExprType, a: Vec<ArgType>) -> SExpression {
        SExpression { expr_type: e, args: a }
    }
}

pub trait OtherEval {
    fn eval(&self, mut env: &mut Frame) -> CalcResult<ArgType>;

    fn to_symbol(&self, env: &mut Frame) -> String;
}

impl OtherEval for SExpression {
    fn eval(&self, env: &mut Frame) -> CalcResult<ArgType> {
        match self.expr_type {
            Operator(super::operator::Add) => test_add(&self.args, env),
            Function(_) => test_user_def("f".to_str(), &self.args, env),
            _ => Err("Not yet defined".to_str())
        }
    }

    fn to_symbol(&self, _: &mut Frame) -> String {
        self.expr_type.to_str()
    }
}

#[deriving(Clone, Show, PartialEq)]
pub enum ArgType {
    Atom(BasicType),
    SExpr(SExpression),
}

#[deriving(Clone, Show, PartialEq)]
pub enum BasicType {
    Boolean(bool),
    BigNum(BigRational),
    Symbol(String),
    Proc(Vec<String>, SExpression),
    List(Vec<LiteralType>),
    Void
}

fn arg_to_basic(arg: &ArgType, env: &mut Frame) -> CalcResult<BasicType> {
    match arg {
        &SExpr(ref x) => arg_to_basic(&try!(x.eval(env)), env),
        &Atom(ref x) => Ok(x.clone())
    }
}

fn get_number(arg: BasicType, env: &mut Frame) -> CalcResult<BigRational> {
    match arg {
        BigNum(x) => Ok(x),
        Symbol(x) => get_number(try!(env.lookup(&x)), env),
        _ => Err("Invalid type for numeric argument.".to_str())
    }
}

fn test_add(args: &Vec<ArgType>, env: &mut Frame) -> CalcResult<ArgType> {
    let mut sum: BigRational = num::zero();
    for arg in args.iter() {
        sum = sum + try!(get_number(try!(arg_to_basic(arg, env)), env));
    }
        
    Ok(Atom(BigNum(sum)))
}

fn test_user_def(name: String, args: &Vec<ArgType>, env: &mut Frame) -> CalcResult<ArgType> {
    let (symbols, fun) = match try!(env.lookup(&name)) {
        Proc(x, y) => (x, y),
        _ => return Err("Not a procedure!".to_str())
    };

    if symbols.len() != args.len() {
        return Err(format!("Bad number of arguments to `{}': expected {} but found {}.",
                           name, symbols.len(), args.len()))
    }

    let mut child_frame = Frame::new_frame(env);
    
    for (symbol, value) in symbols.iter().zip(args.iter()) {
        child_frame.symbols.insert(symbol.clone(), try!(arg_to_basic(value, env)));
    }

    fun.eval(&mut child_frame)
}
 
//Just seeing if this might be a valid path to walk down.
pub fn test() {
    let mut top_frame = Frame::new_global();

    let add = Operator(super::operator::Add);

    let one: BigRational = num::one();
    let two: BigRational = one + one;
    let (eins, zwei) = (Atom(BigNum(one.clone())), Atom(BigNum(two.clone())));

    let x = Atom(Symbol("x".to_str()));

    top_frame.symbols.insert("x".to_str(), BigNum(one + one));

    let new_e = SExpression::new(add.clone(), vec!(eins.clone()));
    assert!(Ok(eins.clone()) == new_e.eval(&mut top_frame));

    let new_r = SExpression::new(add.clone(), vec!(SExpr(new_e.clone()), zwei.clone()));

    let mut answer = new_r.eval(&mut top_frame);
    let mut right = Ok(Atom(BigNum(one + one + one)));
    assert!(answer == right);

    let newest = SExpression::new(add.clone(), vec!(x.clone(), x.clone()));

    answer = newest.eval(&mut top_frame);
    right = Ok(Atom(BigNum(two + two)));
    assert!(answer == right);

    let f_expr = SExpression::new(add, vec!(x.clone(), Atom(BigNum(two * (two + two)))));
    top_frame.symbols.insert("f".to_str(), Proc(vec!("x".to_str()), f_expr));

    let user_def = SExpression::new(Function("f".to_str()), vec!(eins));
    
    answer = user_def.eval(&mut top_frame);
    println!("{}", answer);
    right = Ok(Atom(BigNum((two + one) * (two + one))));
    assert!(answer == right);

    println!("All assertions passed");
}
