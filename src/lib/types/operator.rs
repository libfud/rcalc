//! Operators

use std::fmt;
use std::from_str;

#[cfg(use_fancy)]
use fancy::{LessThanEq, GreaterThanEq};
#[cfg(not(use_fancy)]
use not_fancy::{LessThanEq, GreaterThanEq};

#[cfg(use_fancy)]
mod fancy {
    static LessThanEq: &'static str = "≤";
    static GreaterThanEq: &'static str = "≥";
}

#[cfg(not(use_fancy)]
mod not_fancy {
    static LessThanEq: &'static str = "<=";
    static GreaterThanEq: &'static str = ">=";
}

#[deriving(Clone, PartialOrd, PartialEq)] 
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow
}

impl fmt::Show for Arith {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Add => "+",
            &Sub => "-",
            &Mul => "*",
            &Div => "/",
            &Rem => "%",
        }));
        Ok(())
    }
}

impl from_str::FromStr for Arith {
    fn from_str(s: &str) -> Option<Arith> {
        match s {
            "+" => Some(Add),
            "-" => Some(Sub),
            "*" => Some(Mul),
            "/" => Some(Div),
            "%" => Some(Rem),
            _ => None
        }
    }
}           

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum Transcendental {
    Log, Ln, Exp,
    Sin, Cos, Tan,
    ASin, ACos, ATan,
    SinH, CosH, TanH,
    ASinH, ACosH, ATanH
}

impl fmt::Show for Transcendental {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Log => "log", &Ln => "ln", &Exp => "exp",
            &Sin => "sin", &Cos => "cos", &Tan => "cos",
            &ASin => "asin", &ACos => "acos", &ATan => "atan",
            &SinH => "sinh", &CosH => "cosh", &TanH => "tanh",
            &ASinH => "asinh", &ACosH => "acosh", &ATanH => "atanh"
        }));
        Ok(())
    }
}

impl from_str::FromStr for Transcendental {
    fn from_str(s: &str) -> Option<Transcendental> {
        match s {
            "log" => Some(Log), "ln" => Some(Ln), "exp", Some(Exp),
            "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan),
            "asin" => Some(Asin),  "acos" => Some(ACos), "atan" => Some(ATan),
            "sinh" => Some(SinH), "cosh" => Some(CosH), "tanh" => Some(TanH),
            "asinh" => Some(ASinH), "acosh" => Some(ACosH), "atanh" => Some(ATanH),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum OrderEq {
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq
}

impl fmt::show for OrderEq {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Eq => "=", &NEq => "!=",
            &Lt => "<", &LtEq => LessThanEq,
            &Gt => ">", &GtEq => GreaterThanEq
        }));
        Ok(())
    }
}

impl from_str::FromStr for OrderEq {
    fn from_str(&self) -> Option<OrderEq> {
        match s {
            "<" => Some(Lt), "<=" => Some(LtEq),
            "=" => Some(Eq), "!=" => Some(NEq),
            ">=" => Some(GtEq), ">" => Some(Gt),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum RoundId {
    Round,
    Floor,
    Ceiling,
    Zero,
    Odd,
    Even
}

impl fmt::show for RoundId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Round => "round",
            &Floor => "floor",
            &Ceiling => "ceiling",
            &Zero => "zero?",
            &Odd => "odd?",
            &Even => "even?",
        }));
        Ok(())
    }
}

impl from_str::FromStr for RoundId {
    fn from_str(s: &str) -> Option<RoundId> {
        match s {
            "round" => Some(Round),
            "ceiling" => Some(Ceiling),
            "floor" => Some(Floor),
            "zero?" => Some(Zero),
            "odd?" => Some(Odd), 
            "even?" => Some(Even),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum Gate {
    If,
    And,
    Or,
    Not,
    Xor
}

impl fmt::Show for Gate {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &If => "if",
            &And => "and", &Or => "or",
            &Not => "not", &Xor => "xor"
        }));
        Ok(())
    }
}

impl from_str::FromStr for Gate {
    fn from_str(s: &str) -> Option<Gate> {
        match s {
            "if" => Some(If), "and" => Some(And),
            "or" => Some(Or), "not" => Some(Not),
            "xor" => Some(Xor),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum ListOps {
    List,
    Cons,
    Car, Cdr,
    Cadr, Cddr,
    Caddr, Cdddr,
}

impl fmt::Show for ListOps {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &List => "list", &Cons => "cons",
            &Car => "car", &Cdr => "cdr",
            &Cadr => "cadr", &Cddr => "cddr",
            &Caddr => "caddr", &Cdddr => "cdddr"
        }));
        Ok(())
    }
}

impl from_str::FromStr for ListOps {
    fn from_str(s: &str) -> Option<ListOps> {
        match s {
            "list" => Some(List),
            "cons" => Some(Cons), 
            "car" => Some(Car), "cdr" => Some(Cdr),
            "cadr" => Some(Cadr), "cddr" => Some(Cddr),
            "caddr" => Some(Caddr), "cdddr" => Some(Cdddr),
            _ => None,
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum XForms {
    Map,
    Reduce,
    Filter,
    Sort,
    RangeList
}

impl from_str::FromStr for XForms {
    fn from_str(s: &str) -> Option<XForms> {
        match s {
            "map" => Some(Map),
            "reduce" => Some(Reduce),
            "filter" => Some(Filter),
            "sort" => Some(Sort),
            "range-list" => Some(RangeList),
            _ => None
        }
    }
}

impl fmt::Show for XForms {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Map => "map",
            &Reduce => "reduce",
            &Filter => "filter",
            &Sort => "sort",
            &RangeList => "range-list",
        }));
        Ok(())
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum MatrixOps {
    MakeMatrix,
    MatrixSet,
    MatrixExtend,
}

impl fmt::Show for MatrixOps {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &MakeMatrix => "make-matrix",
            &MatrixSet => "matrix-set",
            &MatrixExtend => "matrix-extend",
        }));
        Ok(())
    }
}

impl from_str::FromStr for MatrixOps {
    fn from_str(s: &str) -> Option<MatrixOps> {
        match s {
            "make-matrix" => Some(MakeMatrix),
            "matrix-extend" => Some(MatrixExtend),
            "matrix-set" => Some(MatrixSet),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq)]
pub enum OperatorType {
    Arithmetic(Arith),
    Pow,
    Transcend(Transcendental),
    Ordering(OrderEq),
    RoundIdent(RoundId),
    Logic(Gate),
    Quote, 
    Listings(ListOps),
    TransForms(XForms),
    Define,
    Lambda,
    Table, 
    MatrixStuff(MatrixOps),
    Help,
}

impl fmt::Show for OperatorType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Arithmetic(ref x) | &Transcend(ref x) | 
            &Ordering(ref x) | &RoundIdent(ref x) |
            &Logic(ref x) | &Listings(ref x) |
            TransForms(ref x) | &MatrixStuff(ref x) => x.to_str(),
            Quote => "'".to_str(),
            Define => "define".to_str(),
            Lambda => "lambda".to_str(),
            Table => "table".to_str(),
            Help => "help".to_str(),
        }));
        Ok(())
    }
}

impl OperatorType {
    pub fn to_ord<T: PartialOrd + PartialEq>(self) -> |T, T| -> bool {
        match self {
            Lt => |a: T, b: T| a < b, LtEq => |a: T, b: T| a <= b,
            Eq => |a: T, b: T| a == b, NEq => |a: T, b: T| a != b,
            GtEq => |a: T, b: T| a >= b, Gt => |a: T, b: T| a > b,
            _ => fail!("Mismatched operator types (don't use ord with nonord)")
        }
    }

    pub fn from_str(s: &str) -> Option<OperatorType> {
        if from_str::<Arithmetic>(s).is_some() {
            return Some(Arithmetic(
        match s {
            "pow" => Some(Pow),
            "define" => Some(Define),
            "lambda" => Some(Lambda),
            "quote" | "'" => Some(Quote),
            "table" => Some(Table),
            "help" => Some(Help),
            _ => None
        }
    }
}

