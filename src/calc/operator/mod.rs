//! Operators

use std::num;
pub use super::{BigRational, Ratio, bigint};
pub use super::{CalcResult, Environment, ArgType, Atom, SExpr};
pub use super::literal::{LiteralType, Lit, LitRes, Symbol, cons, car, cdr, list};

pub mod special;
pub mod power;
pub mod arithmetic;
pub mod logic;
pub mod listops;
pub mod trig;

#[deriving(Show, Clone, PartialOrd, PartialEq)] 
pub enum OperatorType {
    Add, Sub, Mul, Div, Rem, Pow,

    Log, Ln, Exp, Sin, Cos, Tan, ASin, ACos, ATan, SinH, CosH, TanH, ASinH, ACosH, ATanH, 

    Eq, NEq, Lt, LtEq, Gt, GtEq,

    Round, Floor, Ceiling, Zero, Odd, Even,

    If, And, Or, Not, Xor,

    Quote, List, Cons, Car, Cdr,  Cadr, Cddr, Caddr, Cdddr,
    
    Map, Reduce, Filter, ListLen,

    Define, Lambda,

    Table, RangeList, Sort, 

    MakeMatrix, MatrixExtend, MatrixSet,

    Help,
}

type BR<T = BigRational> = T;
impl OperatorType {
    pub fn to_numops(self) -> logic::NumOps {
        match self {
            Round => logic::Round, Floor => logic::Floor,
            Ceiling => logic::Ceiling, Zero => logic::Zero,
            Odd => logic::Odd, Even => logic::Even,
            _ => fail!("Mismatched operator types!")
        }
    }

    pub fn to_ord<T: PartialOrd + PartialEq>(self) -> |T, T| -> bool {
        match self {
            Lt => |a: T, b: T| a < b, LtEq => |a: T, b: T| a <= b,
            Eq => |a: T, b: T| a == b, NEq => |a: T, b: T| a != b,
            GtEq => |a: T, b: T| a >= b, Gt => |a: T, b: T| a > b,
            _ => fail!("Mismatched operator types (don't use ord with nonord)")
        }
    }

    pub fn to_arith(&self) -> |Lit, &Lit| -> LitRes {
        match self {
            &Add => |a: Lit, b: &Lit| a + *b, &Sub => |a: Lit, b: &Lit| a - *b,
            &Mul => |a: Lit, b: &Lit| a * *b, &Div => |a: Lit, b: &Lit| a / *b,
            &Rem => |a: Lit, b: &Lit| a % *b,
            _ => fail!("Mismatched operator types (don't use arith with non-arithmetic")
        }
    }

    pub fn to_arith_args(&self) -> (uint, |Lit, &Lit| -> LitRes, || -> BR) {
        match self {
            &Add => (0, self.to_arith(), || num::zero()),
            &Sub => (1, self.to_arith(), || num::zero()),
            &Mul => (0, self.to_arith(), || num::one()),
            &Div => (1, self.to_arith(), || num::one()),
            &Rem => (1, self.to_arith(), || num::one()),
            _ => fail!("Mistmatched operator types (don't use arith with non-arithmetic")
        }
    }
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+" => Some(Add),  "-" => Some(Sub), "*" => Some(Mul), "/"  => Some(Div),
        "%" => Some(Rem),  "pow" => Some(Pow),

        "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan),
        "asin" => Some(ASin), "acos" => Some(ACos), "atan" => Some(ATan),
        "sinh" => Some(SinH), "cosh" => Some(CosH), "tanh" => Some(TanH), 
        "asinh" => Some(ASinH), "acosh" => Some(ACosH), "atanh" => Some(ATanH),
        "log" => Some(Log), "ln" => Some(Ln), "exp" => Some(Exp),

        "<" => Some(Lt), "<=" => Some(LtEq),
        "=" => Some(Eq), "!=" => Some(NEq),
        ">=" => Some(GtEq), ">" => Some(Gt),

        "if" => Some(If), "and" => Some(And), "or" => Some(Or), "not" => Some(Not),
        "xor" => Some(Xor),

        "round" => Some(Round), "ceiling" => Some(Ceiling), "floor" => Some(Floor),
        "zero?" => Some(Zero), "odd?" => Some(Odd), "even?" => Some(Even),

        "define" => Some(Define), "lambda" => Some(Lambda),

        "quote" | "'"  => Some(Quote), "list" => Some(List),
        "cons" => Some(Cons), "car" => Some(Car), "cdr" => Some(Cdr), 
        "cadr" => Some(Cadr), "cddr" => Some(Cddr), 
        "caddr" => Some(Caddr), "cdddr" => Some(Cdddr),

        "map" => Some(Map), "reduce" => Some(Reduce), "filter" => Some(Filter),
        "list-len" => Some(ListLen),

        "table" => Some(Table), "range-list" => Some(RangeList), 
        "sort" => Some(Sort),

        "make-matrix" => Some(MakeMatrix), "matrix-extend" => Some(MatrixExtend),
        "matrix-set" => Some(MatrixSet),

        "help"  => Some(Help),

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

        Table => "table", RangeList => "range-list", Sort => "sort",

        MakeMatrix => "make-matrix", MatrixExtend => "matrix-extend",
        MatrixSet => "matrix-set",

        Help  => "help",
    };

    answer.to_str()
}

pub fn eval(op_type: OperatorType, args: &Vec<ArgType>, 
            env: &mut Environment) -> CalcResult {
    use self::logic::{num_op, and_or, not, xor, ordering};
    use self::arithmetic::arith;
    use self::listops::{map, filter, reduce, rangelist, listlen};
    use self::special::{table, sort};
    use self::trig::float_ops;
    use super::matrix;

    match op_type {
        Define  => super::define(args, env),
        Lambda => Ok(Atom(super::literal::Void)),

        List => list(args, env),
        Quote => Ok(Atom(super::literal::Void)),
        Cons => cons(args, env), 
        Car => car(args, env), Cdr => cdr(args, env),
        Cadr => car(&vec!(try!(cdr(args, env))), env), 
        Cddr => cdr(&vec!(try!(cdr(args, env))), env),
        Caddr => car(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),
        Cdddr => cdr(&vec!(try!(cdr(&vec!(try!(cdr(args, env))), env))), env),

        Map => listops::map(args, env), Reduce => reduce(args, env),
        Filter => filter(args, env),

        Add | Sub | Mul | Div | Rem => {
            let (min, op, ident) = op_type.to_arith_args();
            arith(args, env, min, op, ident)
        },
        Pow => power::pow_wrapper(args, env),

        Sin | Cos | Tan | ASin |
        ACos | ATan | SinH | CosH | TanH | 
        ASinH | ACosH | ATanH | Log | Ln | Exp => float_ops(args, env, op_type),

        If   => logic::cond(args, env),
        And  => and_or(args, env, false), Or => and_or(args, env, true),
        Not  => not(args, env), Xor  => xor(args, env),

        Lt | LtEq | Eq | NEq | GtEq | Gt => ordering(args, env, op_type.to_ord()),

        Round | Ceiling | Floor | Zero | Even | Odd => num_op(args, env, op_type.to_numops()),

        Table => special::table(args, env),

        RangeList => rangelist(args, env), Sort => sort(args, env), 
        ListLen => listlen(args, env),

        MakeMatrix => matrix::make_matrix(args, env),
        MatrixExtend => matrix::matrix_extend(args, env),
        MatrixSet => matrix::matrix_set(args, env),

        Help => super::common::help(args),
    }
}
