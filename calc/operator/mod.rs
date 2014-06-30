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

    Log, Ln, Exp, Sin, Cos, Tan, ASin, ACos, ATan, SinH, CosH, TanH,
    ASinH, ACosH, ATanH, 

    Eq, NEq, Lt, LtEq, Gt, GtEq,

    Round, Floor, Ceiling, Zero, Odd, Even,

    If, And, Or, Not, Xor,

    Quote, List, Cons, Car, Cdr,  Cadr, Cddr, Caddr, Cdddr,
    
    Map, Reduce, Filter, ListLen,

    Define, Lambda,

    Help, Table, RangeList, Sort, 
}

impl OperatorType {
    pub fn to_numops(self) -> logic::NumOps {
        match self {
            Round => logic::Round,
            Floor => logic::Floor,
            Ceiling => logic::Ceiling,
            Zero => logic::Zero,
            Odd => logic::Odd,
            Even => logic::Even,
            _ => fail!("Mismatched operator types!")
        }
    }
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+" => Some(Add),  "-" => Some(Sub), "*" => Some(Mul), "/"  => Some(Div),
        "%" => Some(Rem),  "pow" => Some(Pow),

        "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan), "asin" => Some(ASin),
        "acos" => Some(ACos), "atan" => Some(ATan), "sinh" => Some(SinH), "cosh" => Some(CosH),
        "tanh" => Some(TanH), "asinh" => Some(ASinH), "acosh" => Some(ACosH),
        "atanh" => Some(ATanH), "log" => Some(Log), "ln" => Some(Ln), "exp" => Some(Exp),

        "<" => Some(Lt), "<=" => Some(LtEq), "=" => Some(Eq), "!=" => Some(NEq),
        ">=" => Some(GtEq), ">" => Some(Gt),

        "if" => Some(If), "and" => Some(And), "or" => Some(Or), "not" => Some(Not),
        "xor" => Some(Xor),

        "round" => Some(Round), "ceiling" => Some(Ceiling), "floor" => Some(Floor),
        "zero?" => Some(Zero), "odd?" => Some(Odd), "even?" => Some(Even),

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

        Sin => "sin", Cos => "cos", Tan => "tan", ASin => "asin", ACos => "acos",
        ATan => "atan", SinH => "sinh", CosH => "cosh", TanH => "tanh", ASinH => "asinh",
        ACosH => "acosh", ATanH => "atanh", Log => "log", Ln => "ln", Exp => "exp",

        Lt => "<", LtEq => "<=", Eq => "=", NEq => "!", GtEq => ">=", Gt => ">",

        If => "if", And => "and", Or => "or", Not => "not", Xor => "xor",

        Round => "round", Ceiling => "ceiling", Floor => "floor",
        Zero => "zero?", Even => "even?", Odd => "odd?",

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
        Define  => super::define(args, env),

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

        Sin | Cos | Tan | ASin | ACos | 
        ATan | SinH | CosH | TanH | ASinH |
        ACosH | ATanH | Log | Ln | Exp => trig::float_ops(args, env, op_type),

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

        Round | Ceiling | Floor | 
        Zero | Even | Odd => logic::num_op(args, env, op_type.to_numops()),

        Help => super::common::help(args),

        Table => special::table(args, env),

        RangeList => listops::rangelist(args, env),

        Sort => special::sort(args, env),
    }
}
