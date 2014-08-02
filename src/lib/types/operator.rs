//! Operators

use std::{from_str, fmt};
use super::record::RecordOps;

#[cfg(use_fancy)]
use self::fancy::{LessThanEq, GreaterThanEq};
#[cfg(not(use_fancy))]
use self::not_fancy::{LessThanEq, GreaterThanEq};

#[cfg(use_fancy)]
mod fancy {
    pub static LessThanEq: &'static str = "≤";
    pub static GreaterThanEq: &'static str = "≥";
}

#[cfg(not(use_fancy))]
mod not_fancy {
    pub static LessThanEq: &'static str = "<=";
    pub static GreaterThanEq: &'static str = ">=";
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)] 
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl fmt::Show for Arith {
    #[inline]
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
    #[inline]
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

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum Transcendental {
    Log, Ln, Exp,
    Sin, Cos, Tan,
    ASin, ACos, ATan,
    SinH, CosH, TanH,
    ASinH, ACosH, ATanH
}

impl fmt::Show for Transcendental {
    #[inline]
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
    #[inline]
    fn from_str(s: &str) -> Option<Transcendental> {
        match s {
            "log" => Some(Log), "ln" => Some(Ln), "exp" => Some(Exp),
            "sin" => Some(Sin), "cos" => Some(Cos), "tan" => Some(Tan),
            "asin" => Some(ASin),  "acos" => Some(ACos), "atan" => Some(ATan),
            "sinh" => Some(SinH), "cosh" => Some(CosH), "tanh" => Some(TanH),
            "asinh" => Some(ASinH), "acosh" => Some(ACosH), "atanh" => Some(ATanH),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum OrderEq {
    Eq,
    NEq,
    Lt,
    LtEq,
    Gt,
    GtEq
}

impl fmt::Show for OrderEq {
    #[inline]
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
    #[inline]
    fn from_str(s: &str) -> Option<OrderEq> {
        match s {
            "<" => Some(Lt), "<=" => Some(LtEq),
            "=" => Some(Eq), "!=" => Some(NEq),
            ">=" => Some(GtEq), ">" => Some(Gt),
            _ => None
        }
    }
}

impl<'a> OrderEq {
    #[inline]
    pub fn to_ord<'a, T: PartialOrd + PartialEq>(self) -> |T, T|:'a -> bool {
        match self {
            Eq => |a: T, b: T| a == b,
            NEq => |a: T, b: T| a != b,
            Lt => |a: T, b: T| a < b,
            LtEq => |a: T, b: T| a <= b,
            Gt => |a: T, b: T| a > b,
            GtEq => |a: T, b: T| a >= b,
         }
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum RoundId {
    Round,
    RoundToNearest,
    Floor,
    Ceiling,
    Zero,
    Odd,
    Even
}

impl fmt::Show for RoundId {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Round => "round",
            &RoundToNearest => "round-to-nearest",
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
    #[inline]
    fn from_str(s: &str) -> Option<RoundId> {
        match s {
            "round" => Some(Round),
            "round-to-nearest" => Some(RoundToNearest),
            "ceiling" => Some(Ceiling),
            "floor" => Some(Floor),
            "zero?" => Some(Zero),
            "odd?" => Some(Odd), 
            "even?" => Some(Even),
            _ => None
        }
    }
}

impl RoundId {
    #[inline]
    pub fn idea(self) -> String {
        let s = match self {
            Round => "be rounded",
            RoundToNearest => "be rounded to the nearest n",
            Floor => "have their floor returned",
            Ceiling => "have their ceiling returned",
            Even => "be even",
            Odd => "be odd",
            Zero => "be zero",
        };

        s.to_string()
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum Gate {
    If,
    And,
    Or,
    Not,
    Xor
}

impl fmt::Show for Gate {
    #[inline]
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
    #[inline]
    fn from_str(s: &str) -> Option<Gate> {
        match s {
            "if" => Some(If), "and" => Some(And),
            "or" => Some(Or), "not" => Some(Not),
            "xor" => Some(Xor),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum ListOps {
    List,
    Cons,
    Car, Cdr,
    Cadr, Cddr,
    Caddr, Cdddr,
}

impl fmt::Show for ListOps {
    #[inline]
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
    #[inline]
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

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum XForms {
    Map,
    Fold,
    FoldR,
    Reduce,
    Filter,
    FilterMap,
    Sort,
    RangeList,
    Reverse
}

impl from_str::FromStr for XForms {
    #[inline]
    fn from_str(s: &str) -> Option<XForms> {
        match s {
            "map" => Some(Map),
            "fold" => Some(Fold),
            "foldr" => Some(FoldR),
            "reduce" => Some(Reduce),
            "filter" => Some(Filter),
            "filter-map" => Some(FilterMap),
            "sort" => Some(Sort),
            "range-list" => Some(RangeList),
            "reverse" => Some(Reverse),
            _ => None
        }
    }
}

impl fmt::Show for XForms {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            Map => "map",
            Fold => "fold",
            FoldR => "foldr",
            Reduce => "reduce",
            Filter => "filter",
            FilterMap => "filter-map",
            Sort => "sort",
            RangeList => "range-list",
            Reverse => "reverse",
        }));
        Ok(())
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum MatrixOps {
    MakeMatrix, MatrixFromFn,

    MatrixSetRow, MatrixSetCol,

    MatrixAppendRows, MatrixAppendCols,

    MatrixConcatRows, MatrixConcatCols,

    MatrixGetElem, MatrixGetRow, MatrixGetCol,

    Scalar, CrossProd, DotProd, Transpose, Translate,

    Determ, MatrixInv,

    PolygonArea,
}

impl fmt::Show for MatrixOps {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            MakeMatrix => "make-matrix",
            MatrixSetRow => "matrix-set-row",
            MatrixSetCol => "matrix-set-col",
            MatrixAppendRows => "matrix-append-rows",
            MatrixAppendCols => "matrix-append-cols",
            MatrixConcatRows => "matrix-concat-rows",
            MatrixConcatCols => "matrix-concat-cols",
            MatrixGetElem => "matrix-get-elem",
            MatrixGetRow => "matrix-get-row",
            MatrixGetCol => "matrix-get-col",
            Determ => "matrix-det",
            Scalar => "matrix-scalar",
            CrossProd => "cross-*",
            DotProd => "dot-*",
            Transpose => "matrix-transpose",
            Translate => "matrix-translate",
            MatrixInv => "matrix-inv",
            MatrixFromFn => "matrix-from-fn",
            PolygonArea => "polygon-area",
        }));
        Ok(())
    }
}

impl from_str::FromStr for MatrixOps {
    #[inline]
    fn from_str(s: &str) -> Option<MatrixOps> {
        match s {
            "make-matrix" => Some(MakeMatrix),
            "matrix-append-rows" => Some(MatrixAppendRows),
            "matrix-append-cols" => Some(MatrixAppendCols),
            "matrix-concat-rows" => Some(MatrixConcatRows),
            "matrix-concat-cols" => Some(MatrixConcatCols),
            "matrix-set-row" => Some(MatrixSetRow),
            "matrix-set-col" => Some(MatrixSetCol),
            "matrix-get-elem" => Some(MatrixGetElem),
            "matrix-get-row" => Some(MatrixGetRow),
            "matrix-get-col" => Some(MatrixGetCol),
            "matrix-det" => Some(Determ),
            "matrix-scalar" => Some(Scalar),
            "cross-*" => Some(CrossProd),
            "dot-*" => Some(DotProd),
            "matrix-inv" => Some(MatrixInv),
            "matrix-transpose" => Some(Transpose),
            "matrix-translate" => Some(Translate),
            "matrix-from-fn" => Some(MatrixFromFn),
            "polygon-area" => Some(PolygonArea),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum Introspect {
    BoolQ,
    LambdaQ,
    ListQ,
    MatrixQ,
    NumberQ,
    StructQ,
    SymbolQ,
}

impl fmt::Show for Introspect {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            BoolQ => "bool?",
            ListQ => "list?",
            MatrixQ => "matrix?",
            NumberQ => "number?",
            LambdaQ => "lambda?",
            StructQ => "struct?",
            SymbolQ => "symbol?",
        }));
        Ok(())
    }
}

impl from_str::FromStr for Introspect {
    #[inline]
    fn from_str(s: &str) -> Option<Introspect> {
        match s {
            "symbol?" => Some(SymbolQ),
            "number?" => Some(NumberQ),
            "bool?" => Some(BoolQ),
            "matrix?" => Some(MatrixQ),
            "list?" => Some(ListQ),
            "lambda?" => Some(LambdaQ),
            "struct?" => Some(StructQ),
            _ => None
        }
    }
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)]
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
    TableFromMatrix,
    MatrixStuff(MatrixOps),
    RecOps(RecordOps),
    Query(Introspect),
    TextGraph,
    Help,
}

impl fmt::Show for OperatorType {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            Arithmetic(ref x) => x.to_string(),
            Transcend(ref x) => x.to_string(),
            Ordering(ref x) => x.to_string(),
            RoundIdent(ref x) => x.to_string(),
            Logic(ref x) => x.to_string(),
            Listings(ref x) => x.to_string(),
            TransForms(ref x) => x.to_string(),
            MatrixStuff(ref x) => x.to_string(),
            RecOps(ref x) => x.to_string(),
            Pow => "pow".to_string(),
            Quote => "'".to_string(),
            Define => "define".to_string(),
            Lambda => "lambda".to_string(),
            Table => "table".to_string(),
            TableFromMatrix => "table-from-matrix".to_string(),
            Query(ref x) => x.to_string(),
            TextGraph => "text-graph".to_string(),
            Help => "help".to_string(),
        }));
        Ok(())
    }
}

impl from_str::FromStr for OperatorType {
    #[inline]
    fn from_str(s: &str) -> Option<OperatorType> {
        match from_str::<Arith>(s) {
            Some(x) => return Some(Arithmetic(x)),
            None => { }
        }

        match from_str::<Transcendental>(s) {
            Some(x) => return Some(Transcend(x)),
            None => { }
        }

        match from_str::<OrderEq>(s) {
            Some(x) => return Some(Ordering(x)),
            None => { }
        }
        
        match from_str::<Gate>(s) {
            Some(x) => return Some(Logic(x)),
            None => { }
        }
        
        match from_str::<ListOps>(s) {
            Some(x) => return Some(Listings(x)),
            None => { }
        }

        match from_str::<XForms>(s) {
            Some(x) => return Some(TransForms(x)),
            None => { }
        }

        match from_str::<MatrixOps>(s) {
            Some(x) => return Some(MatrixStuff(x)),
            None => { }
        }

        match from_str::<RoundId>(s) {
            Some(x) => return Some(RoundIdent(x)),
            None => { }
        }

        match from_str::<RecordOps>(s) {
            Some(x) => return Some(RecOps(x)),
            None => { }
        }

        match from_str::<Introspect>(s) {
            Some(x) => return Some(Query(x)),
            None => { }
        }
    
        match s {
            "pow" => Some(Pow),
            "define" => Some(Define),
            "lambda" => Some(Lambda),
            "quote" | "'" => Some(Quote),
            "table" => Some(Table),
            "table-from-matrix" => Some(TableFromMatrix),
            "text-graph" => Some(TextGraph),
            "help" => Some(Help),
            _ => None
        }
    }
}
