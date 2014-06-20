//! Operators

use std::num;
pub use super::{Evaluate, CalcResult, Environment, ArgType, Atom, SExpr, arg_to_literal};
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

    If, And, Or, Not,

    Quote, List, Cons, Car, Cdr, Map, Reduce, Filter,

    Define, Lambda,

    Help, Table, TableList, RangeList, Sort,
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+" => Some(Add),  "-" => Some(Sub), "*" => Some(Mul), "/"  => Some(Div),
        "%" => Some(Rem),  "pow" => Some(Pow),

        "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan),

        "<" => Some(Lt), "<=" => Some(LtEq), "=" => Some(Eq), "!=" => Some(NEq),
        ">=" => Some(GtEq), ">" => Some(Gt),

        "if" => Some(If), "and" => Some(And), "or" => Some(Or), "not" => Some(Not),

        "define" => Some(Define), "lambda" => Some(Lambda),

        "quote" | "'"  => Some(Quote), "list" => Some(List),  "cons" => Some(Cons),
        "car" => Some(Car), "cdr" => Some(Cdr), "map" => Some(Map),
        "reduce" => Some(Reduce), "filter" => Some(Filter),

        "help"  => Some(Help), "table" => Some(Table), "table-list" => Some(TableList),
        "range-list" => Some(RangeList), "sort" => Some(Sort),

        _       => None
    }
}

pub fn to_str(op: &OperatorType) -> String {
    let answer = match *op {
        Add => "+", Sub => "-", Mul => "*", Div => "/", Rem => "%", Pow => "pow",

        Sin => "sin", Cos => "cos", Tan => "tan",

        Lt => "<", LtEq => "<=", Eq => "=", NEq => "!", GtEq => ">=", Gt => ">",

        If => "if", And => "and", Or => "or", Not => "not", 

        Define => "define", Lambda => "lambda",

        Quote => "quote", List => "list", Cons => "cons", Car => "car", 
        Cdr => "cdr", Map => "map", Reduce => "reduce", Filter => "filter",

        Help  => "help", Table => "table", TableList => "table-list", 
        RangeList => "range-list", Sort => "sort",
    };

    answer.to_str()
}

pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, 
            env: &mut Environment) -> CalcResult {
    match op_type {
        Define  => Ok(super::literal::Void),

        Lambda => Ok(super::literal::Void),

        List => list(args, env),

        Quote => Ok(super::literal::Void),

        Cons => cons(args, env),

        Car => car(args, env),

        Cdr => cdr(args, env),

        Map => listops::map(args, env),

        Reduce => listops::reduce(args, env),

        Filter => listops::filter(args, env),

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

        Lt   => logic::ordering(args, env, |a, b| a < b),

        LtEq => logic::ordering(args, env, |a, b| a <= b),

        Eq   => logic::equality(args, env, true),

        NEq  => logic::equality(args, env, false),

        GtEq => logic::ordering(args, env, |a, b| a >= b),
        
        Gt   => logic::ordering(args, env, |a, b| a > b),

        Help => super::common::help(args, env),

        Table => special::table(args, env),

        TableList => special::table_list(args, env),

        RangeList => listops::rangelist(args, env),
        
        Sort => special::sort(args, env),
    }
}
