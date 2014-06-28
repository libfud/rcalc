//! Operators

use std::num;
pub use super::{BigRational, Ratio, bigint};
pub use super::{CalcResult, Environment, ArgType, Atom, SExpr};
pub use super::literal::{LiteralType, Symbol, cons, car, cdr, list};

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

#[deriving(Show, Clone, PartialOrd, PartialEq)] 
pub enum OperatorType {
    Add, Sub, Mul, Div, Rem, Pow,

    Sin, Cos, Tan,

    Eq, NEq, Lt, LtEq, Gt, GtEq,

    If, And, Or, Not, Xor,

    Quote, List, Cons, Car, Cdr,  Cadr, Cddr, Caddr, Cdddr,
    
    Map, Reduce, Filter, ListLen,

    Define, Lambda,

    Help, Table, RangeList, Sort, 
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+" => Some(Add),  "-" => Some(Sub), "*" => Some(Mul), "/"  => Some(Div),
        "%" => Some(Rem),  "pow" => Some(Pow),

        "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan),

        "<" => Some(Lt), "<=" => Some(LtEq), "=" => Some(Eq), "!=" => Some(NEq),
        ">=" => Some(GtEq), ">" => Some(Gt),

        "if" => Some(If), "and" => Some(And), "or" => Some(Or), "not" => Some(Not),
        "xor" => Some(Xor),

        "define" => Some(Define), "lambda" => Some(Lambda),

        "quote" | "'"  => Some(Quote), "list" => Some(List),  "cons" => Some(Cons),
        "car" => Some(Car), "cdr" => Some(Cdr), "cadr" => Some(Cadr),
        "cddr" => Some(Cddr), "caddr" => Some(Caddr), "cdddr" => Some(Cdddr),


        "map" => Some(Map), "reduce" => Some(Reduce), "filter" => Some(Filter),
        "list-len" => Some(ListLen),

        "help"  => Some(Help), "table" => Some(Table),
        "range-list" => Some(RangeList), "sort" => Some(Sort),

        _       => None
    }
}

pub fn to_str(op: &OperatorType) -> String {
    let answer = match *op {
        Add => "+", Sub => "-", Mul => "*", Div => "/", Rem => "%", Pow => "pow",

        Sin => "sin", Cos => "cos", Tan => "tan",

        Lt => "<", LtEq => "<=", Eq => "=", NEq => "!", GtEq => ">=", Gt => ">",

        If => "if", And => "and", Or => "or", Not => "not", Xor => "xor",

        Define => "define", Lambda => "lambda",

        Quote => "quote", List => "list", Cons => "cons", Car => "car", 
        Cdr => "cdr",  Cadr => "cadr", Cddr => "cddr", Caddr => "caddr",
        Cdddr => "cdddr",

        Map => "map", Reduce => "reduce", Filter => "filter", ListLen => "list-len",

        Help  => "help", Table => "table", RangeList => "range-list", 
        Sort => "sort",
    };

    answer.to_str()
}

pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, 
            env: &mut Environment) -> CalcResult {
    match op_type {
        Define  => Ok(Atom(super::literal::Void)),

        Lambda => Ok(Atom(super::literal::Void)),

        List => list(args, env),

        Quote => Ok(Atom(super::literal::Void)),

        Cons => cons(args, env),

        Car => car(args, env),

        Cdr => cdr(args, env),

        Cadr => car(&vec!(try!(cdr(args, env))), env),

        Cddr => cdr(&vec!(try!(cdr(args, env))), env),

        Caddr => car(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),

        Cdddr => cdr(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),

        Map => listops::map(args, env),

        Reduce => listops::reduce(args, env),

        Filter => listops::filter(args, env),

        ListLen => listops::listlen(args, env),

        Add => arithmetic::do_op(args, env, 0, |a, b| a + *b, num::zero),

        Sub => arithmetic::do_op(args, env, 1, |a, b| a - *b, num::zero),

        Mul => arithmetic::do_op(args, env, 0, |a, b| a * *b, num::one),

        Div => arithmetic::divrem(args, env, |a, b| a / *b), 
        //division can fail with zeros

        Rem => arithmetic::divrem(args, env, |a, b| a % *b),

        Pow => power::pow_wrapper(args, env),

        Sin => trig::trig(args, env, |x| x.sin()),

        Cos => trig::trig(args, env, |x| x.cos()),

        Tan => trig::trig(args, env, |x| x.tan()),

        If   => logic::cond(args, env),

        And  => logic::and_or(args, env, false),

        Or   => logic::and_or(args, env, true),

        Not  => logic::not(args, env),

        Xor  => logic::xor(args, env),

        Lt   => logic::ordering(args, env, |a, b| a < b),

        LtEq => logic::ordering(args, env, |a, b| a <= b),

        Eq   => logic::ordering(args, env, |a, b| a == b),

        NEq  => logic::ordering(args, env, |a, b| a != b),

        GtEq => logic::ordering(args, env, |a, b| a >= b),
        
        Gt   => logic::ordering(args, env, |a, b| a > b),

        Help => super::common::help(args),

        Table => special::table(args, env),

        RangeList => listops::rangelist(args, env),

        Sort => special::sort(args, env),
    }
}
