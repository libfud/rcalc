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

pub trait Help {
    fn help<'a>(&self) -> &'a str;
}

#[deriving(Clone, PartialOrd, PartialEq, Eq, Ord)] 
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

impl Help for Arith {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Add => "The addition operator. For 0 terms, returns the the additive identity.",
            Sub => "The subtraction operator. Requires at least one term. If only one term is
given, it returns the negation of that term.",
            Mul => "The multiplication operator. For 0 terms, returns the multiplicative identity.",
            Div => "The division operator. Requires at least one term. If only one term is
given, it returns its reciprocal.",
            Rem => "The remainder operator. Requires two terms."
        }
    }
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

impl Help for Transcendental {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Log => "The common logarithm. Takes one term.",
            Ln => "The natural logarithm. Takes one term.",
            Exp => "Natural exponentiation. Takes one term.",
            Sin => "The sine function. Takes one term, expressed in radians.",
            Cos => "The cosine function. Takes one term, expressed in radians.",
            Tan => "The tangent function. Takes one term, expressed in radians.",
            ASin => "The inverse sine function. Takes one term in the range [-pi/2, pi/2].",
            ACos => "The inverse cosine function. Takes one term in the range [-pi/2, pi/2].",
            ATan => "The inverse tangent  function. Takes one term in the range [-pi/2, pi/2].",
            SinH => "The hyperbolic sine function. Takes one term.",
            CosH => "The hyperbolic cosine function. Takes one term.",
            TanH => "The hyperbolic tangent function. Takes one term.",
            ASinH => "The inverse hyperbolic sine function. Takes one term in the range [-pi/2, pi/2].",
            ACosH => "The inverse hyperbolic cosine function. Takes one term in the range [-pi/2, pi/2].",
            ATanH => "The inverse hyperbolic tangent function. Takes one term in the range [-pi/2, pi/2]."
        }
    }
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

impl Help for OrderEq {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Lt => "The ordering less than. Takes two terms",
            LtEq => "The ordering less than or equal. Takes two terms.",
            Eq => "Equality. Takes two terms, and returns either true or false.",
            NEq => "Inequality. Takes two terms, and returns either true or false.",
            GtEq => "The ordering greater than or equal. Takes two terms.",
            Gt => "The ordering greater than. Takes two terms."
        }
    }
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

impl Help for RoundId {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Round => "Round takes one term. Rounds away from zero.",
            RoundToNearest => {
                "Round to the nearest x. Takes two terms; the first is the number to be rounded,
 and the second is the number towards which it is rounded."
            },
            Floor => {
                "Floor takes one term, and returns the nearest integer whose absolute value
 is less than the absolute of the term."
            },
            Ceiling => {
                "Ceiling takes one term, and returns the nearest integer whose absolute value
 is greater than the absolute of the term."
            },
            Zero => "zero? checks to see if the term is zero or not. Takes one term.",
            Odd => "odd? checks to see if the term is odd or not. Takes one term.",
            Even => "even? checks to see if the term is even or not. Takes one term."
        }
    }
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

impl Help for Gate {
    fn help<'a>(&self) -> &'a str {
        match *self {
            If => "if takes three expressions as arguments: a conditional statement, a consequent, 
and an alternative.",
            And => "The logical And operator. Returns true if all terms are true.",
            Or => "The logical Or operator. Returns true if any terms are true.",
            Not => "The logical Not operator. Returns the opposite of the term.",
            Xor => "The logical Xor operator. Returns true iaoi one of two terms it is comparing are true."
        }
    }
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

impl Help for ListOps {
    fn help<'a>(&self) -> &'a str {
        match *self {
            List => "list returns a list from its arguments, taking any amount of terms.",
            Cons => "cons returns a list from its arguments, taking two terms.",
            Car => "car returns the first element of a list. Takes one term.",
            Cdr => "cdr returns the rest of a list. Takes one term.",
            Cadr => "cadr returns the the second element of a list if its length is at least 2.",
            Cddr => "cddr returns the tail of a list after the second element.",
            Caddr => "caddr returns the third element of a list.",
            Cdddr => "cdddr returns the tail of a list after the third element.",
        }
    }
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

impl Help for XForms {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Map => "Map a function to each element of a list. The first argument is a function 
with as many terms as there are lists. For example, if there are four lists, 
the function must have four terms. Lists are traversed from left to right.
Takes at least two terms, with every term after the first being a list.",
            Fold => "The higher order function fold. Takes a function with two arguments, 
an initial value and a list. If there are no terms in the list, returns the initial value. 
If there is only one term in the list, it returns the first element in the list. 
Lists are folded from left to right.",
            FoldR => "The higher order function fold-right. Takes a function wtih two arguments, 
an initial value and a list. If there are no terms terms in the list, returns the initial value. 
If there is only one term in the list, it returns the first element in the list. 
Lists are folded from right to left.",
            Reduce => "The higher order function reduce. Takes a function with two arguments, 
an initial value and a list with at least one term. If there is only one term in the list, 
it returns that. Lists are folded from left to right.",
            Filter => "The higher order function filter. Takes a predicate with one argument and a list.
Returns a new list whose elements are comprised of members of the original list 
for which the predicate is true.",
            FilterMap => "The high order function filter-map. Takes a predicate with one argument, 
a function with one argument, and a list. Returns a new list whose elemenets are
comprised of members of the original list for which the predicate is true 
and to which the mapping function has been applied.",
            Sort => "Sorts a list from least to greatest values. Takes one list.",
            RangeList => "Returns a list, taking a beginning (a), an end (b), and optionally a step. 
The default step is one, returning the range (a, b].",
            Reverse => "Reverses a list. Takes one argument."
        }
    }
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

impl Help for MatrixOps {
    fn help<'a>(&self) -> MatrixOps {
        MakeMatrix => "make-matrix takes one argument, a list of lists. Each list represents a row,
and every list must be of equal length.",
        

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

impl Help for OperatorType {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Arithmetic(x) => x.help(),
            Transcend(x) => x.help(),
            _ => "lol"
        }
    }
}
