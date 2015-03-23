//! Operators

use std::fmt;
use std::str::FromStr;
use super::record::RecordOps;

#[cfg(use_fancy)]
use self::fancy::{LESS_THAN_EQ, GREATER_THAN_EQ};
#[cfg(not(use_fancy))]
use self::not_fancy::{LESS_THAN_EQ, GREATER_THAN_EQ};

#[cfg(use_fancy)]
mod fancy {
    pub static LESS_THAN_EQ: &'static str = "≤";
    pub static GREATER_THAN_EQ: &'static str = "≥";
}

#[cfg(not(use_fancy))]
mod not_fancy {
    pub static LESS_THAN_EQ: &'static str = "<=";
    pub static GREATER_THAN_EQ: &'static str = ">=";
}

pub trait Help {
    fn help<'a>(&self) -> &'a str;
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)] 
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
            Arith::Add => "The addition operator. For 0 terms, returns the the additive identity.",
            Arith::Sub => "The subtraction operator. Requires at least one term. If only one term is
given, it returns the negation of that term.",
            Arith::Mul => "The multiplication operator. For 0 terms, returns the multiplicative identity.",
            Arith::Div => "The division operator. Requires at least one term. If only one term is
given, it returns its reciprocal.",
            Arith::Rem => "The remainder operator. Requires two terms."
        }
    }
}

impl fmt::Display for Arith {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Arith::Add => "+",
            &Arith::Sub => "-",
            &Arith::Mul => "*",
            &Arith::Div => "/",
            &Arith::Rem => "%",
        }));
        Ok(())
    }
}

impl FromStr for Arith {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<Arith,()> {
        match s {
            "+" => Ok(Arith::Add),
            "-" => Ok(Arith::Sub),
            "*" => Ok(Arith::Mul),
            "/" => Ok(Arith::Div),
            "%" => Ok(Arith::Rem),
            _ => Err(())
        }
    }
}           

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            Transcendental::Log => "The common logarithm. Takes one term.",
            Transcendental::Ln => "The natural logarithm. Takes one term.",
            Transcendental::Exp => "Natural exponentiation. Takes one term.",
            Transcendental::Sin => "The sine function. Takes one term, expressed in radians.",
            Transcendental::Cos => "The cosine function. Takes one term, expressed in radians.",
            Transcendental::Tan => "The tangent function. Takes one term, expressed in radians.",
            Transcendental::ASin => "The inverse sine function. Takes one term in the range [-pi/2, pi/2].",
            Transcendental::ACos => "The inverse cosine function. Takes one term in the range [-pi/2, pi/2].",
            Transcendental::ATan => "The inverse tangent  function. Takes one term in the range [-pi/2, pi/2].",
            Transcendental::SinH => "The hyperbolic sine function. Takes one term.",
            Transcendental::CosH => "The hyperbolic cosine function. Takes one term.",
            Transcendental::TanH => "The hyperbolic tangent function. Takes one term.",
            Transcendental::ASinH => "The inverse hyperbolic sine function. Takes one term in the range [-pi/2, pi/2].",
            Transcendental::ACosH => "The inverse hyperbolic cosine function. Takes one term in the range [-pi/2, pi/2].",
            Transcendental::ATanH => "The inverse hyperbolic tangent function. Takes one term in the range [-pi/2, pi/2]."
        }
    }
}

impl fmt::Display for Transcendental {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Transcendental::Log => "log",
            &Transcendental::Ln => "ln",
            &Transcendental::Exp => "exp",
            &Transcendental::Sin => "sin",
            &Transcendental::Cos => "cos", 
            &Transcendental::Tan => "cos",
            &Transcendental::ASin => "asin",
            &Transcendental::ACos => "acos",
            &Transcendental::ATan => "atan",
            &Transcendental::SinH => "sinh",
            &Transcendental::CosH => "cosh",
            &Transcendental::TanH => "tanh",
            &Transcendental::ASinH => "asinh",
            &Transcendental::ACosH => "acosh", 
            &Transcendental::ATanH => "atanh"
        }));
        Ok(())
    }
}

impl FromStr for Transcendental {
    #[inline]
    type Err= ();
    fn from_str(s: &str) -> Result<Transcendental,()> {
        match s {
            "log"   => Ok(Transcendental::Log), 
            "ln"    => Ok(Transcendental::Ln), 
            "exp"   => Ok(Transcendental::Exp),
            "sin"   => Ok(Transcendental::Sin), 
            "cos"   => Ok(Transcendental::Cos), 
            "tan"   => Ok(Transcendental::Tan),
            "asin"  => Ok(Transcendental::ASin),
            "acos"  => Ok(Transcendental::ACos),
            "atan"  => Ok(Transcendental::ATan),
            "sinh"  => Ok(Transcendental::SinH), 
            "cosh"  => Ok(Transcendental::CosH), 
            "tanh"  => Ok(Transcendental::TanH),
            "asinh" => Ok(Transcendental::ASinH), 
            "acosh" => Ok(Transcendental::ACosH), 
            "atanh" => Ok(Transcendental::ATanH),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            OrderEq::Lt => "The ordering less than. Takes two terms",
            OrderEq::LtEq => "The ordering less than or equal. Takes two terms.",
            OrderEq::Eq => "Equality. Takes two terms, and returns either true or false.",
            OrderEq::NEq => "Inequality. Takes two terms, and returns either true or false.",
            OrderEq::GtEq => "The ordering greater than or equal. Takes two terms.",
            OrderEq::Gt => "The ordering greater than. Takes two terms."
        }
    }
}

impl fmt::Display for OrderEq {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &OrderEq::Eq => "=", &OrderEq::NEq => "!=",
            &OrderEq::Lt => "<", &OrderEq::LtEq => LESS_THAN_EQ,
            &OrderEq::Gt => ">", &OrderEq::GtEq => GREATER_THAN_EQ
        }));
        Ok(())
    }
}

impl FromStr for OrderEq {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<OrderEq,()> {
        match s {
            "<"  => Ok(OrderEq::Lt),
            "<=" => Ok(OrderEq::LtEq),
            "="  => Ok(OrderEq::Eq), 
            "!=" => Ok(OrderEq::NEq),
            ">=" => Ok(OrderEq::GtEq),
            ">"  => Ok(OrderEq::Gt),
            _ => Err(())
        }
    }
}

impl<'a> OrderEq {
    #[inline]
    pub fn to_ord<T: PartialOrd + PartialEq>(self) -> Box<Fn(T, T) -> bool> {
        match self {
            OrderEq::Eq => Box::new(move |a: T, b: T| a == b),
            OrderEq::NEq => Box::new(move |a: T, b: T| a != b),
            OrderEq::Lt => Box::new(move |a: T, b: T| a < b),
            OrderEq::LtEq => Box::new(move |a: T, b: T| a <= b),
            OrderEq::Gt => Box::new(move |a: T, b: T| a > b),
            OrderEq::GtEq => Box::new(move |a: T, b: T| a >= b),
         }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            RoundId::Round => "Round takes one term. Rounds away from zero.",
            RoundId::RoundToNearest => {
                "Round to the nearest x. Takes two terms; the first is the number to be rounded,
 and the second is the number towards which it is rounded."
            },
            RoundId::Floor => {
                "RoundId::Floor takes one term, and returns the nearest integer whose absolute value
 is less than the absolute of the term."
            },
            RoundId::Ceiling => {
                "RoundId::Ceiling takes one term, and returns the nearest integer whose absolute value
 is greater than the absolute of the term."
            },
            RoundId::Zero => "zero? checks to see if the term is zero or not. Takes one term.",
            RoundId::Odd => "odd? checks to see if the term is odd or not. Takes one term.",
            RoundId::Even => "even? checks to see if the term is even or not. Takes one term."
        }
    }
}

impl fmt::Display for RoundId {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &RoundId::Round => "round",
            &RoundId::RoundToNearest => "round-to-nearest",
            &RoundId::Floor => "floor",
            &RoundId::Ceiling => "ceiling",
            &RoundId::Zero => "zero?",
            &RoundId::Odd => "odd?",
            &RoundId::Even => "even?",
        }));
        Ok(())
    }
}

impl FromStr for RoundId {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<RoundId,()> {
        match s {
            "round" => Ok(RoundId::Round),
            "round-to-nearest" => Ok(RoundId::RoundToNearest),
            "ceiling" => Ok(RoundId::Ceiling),
            "floor" => Ok(RoundId::Floor),
            "zero?" => Ok(RoundId::Zero),
            "odd?" => Ok(RoundId::Odd), 
            "even?" => Ok(RoundId::Even),
            _ => Err(())
        }
    }
}

impl RoundId {
    #[inline]
    pub fn idea(self) -> String {
        let s = match self {
            RoundId::Round => "be rounded",
            RoundId::RoundToNearest => "be rounded to the nearest n",
            RoundId::Floor => "have their floor returned",
            RoundId::Ceiling => "have their ceiling returned",
            RoundId::Even => "be even",
            RoundId::Odd => "be odd",
            RoundId::Zero => "be zero",
        };

        s.to_string()
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            Gate::If => "if takes three expressions as arguments: a conditional statement, a consequent, 
and an alternative.",
            Gate::And => "The logical And operator. Returns true if all terms are true.",
            Gate::Or => "The logical Or operator. Returns true if any terms are true.",
            Gate::Not => "The logical Not operator. Returns the opposite of the term.",
            Gate::Xor => "The logical Xor operator. Returns true iaoi one of two terms it is comparing are true."
        }
    }
}

impl fmt::Display for Gate {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &Gate::If => "if",
            &Gate::And => "and", &Gate::Or => "or",
            &Gate::Not => "not", &Gate::Xor => "xor"
        }));
        Ok(())
    }
}

impl FromStr for Gate {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<Gate,()> {
        match s {
            "if"  => Ok(Gate::If),
            "and" => Ok(Gate::And),
            "or"  => Ok(Gate::Or), 
            "not" => Ok(Gate::Not),
            "xor" => Ok(Gate::Xor),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            ListOps::List => "list returns a list from its arguments, taking any amount of terms.",
            ListOps::Cons => "cons returns a list from its arguments, taking two terms.",
            ListOps::Car => "car returns the first element of a list. Takes one term.",
            ListOps::Cdr => "cdr returns the rest of a list. Takes one term.",
            ListOps::Cadr => "cadr returns the the second element of a list if its length is at least 2.",
            ListOps::Cddr => "cddr returns the tail of a list after the second element.",
            ListOps::Caddr => "caddr returns the third element of a list.",
            ListOps::Cdddr => "cdddr returns the tail of a list after the third element.",
        }
    }
}

impl fmt::Display for ListOps {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match self {
            &ListOps::List => "list", 
            &ListOps::Cons => "cons",
            &ListOps::Car => "car", 
            &ListOps::Cdr => "cdr",
            &ListOps::Cadr => "cadr",
            &ListOps::Cddr => "cddr",
            &ListOps::Caddr => "caddr", 
            &ListOps::Cdddr => "cdddr"
        }));
        Ok(())
    }
}

impl FromStr for ListOps {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<ListOps,()> {
        match s {
            "list"  => Ok(ListOps::List),
            "cons"  => Ok(ListOps::Cons), 
            "car"   => Ok(ListOps::Car), 
            "cdr"   => Ok(ListOps::Cdr),
            "cadr"  => Ok(ListOps::Cadr),
            "cddr"  => Ok(ListOps::Cddr),
            "caddr" => Ok(ListOps::Caddr), 
            "cdddr" => Ok(ListOps::Cdddr),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
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
            XForms::Map => "Map a function to each element of a list. The first argument is a function 
with as many terms as there are lists. For example, if there are four lists, 
the function must have four terms. Lists are traversed from left to right.
Takes at least two terms, with every term after the first being a list.",
            XForms::Fold => "The higher order function fold. Takes a function with two arguments, 
an initial value and a list. If there are no terms in the list, returns the initial value. 
If there is only one term in the list, it returns the first element in the list. 
Lists are folded from left to right.",
            XForms::FoldR => "The higher order function fold-right. Takes a function wtih two arguments, 
an initial value and a list. If there are no terms terms in the list, returns the initial value. 
If there is only one term in the list, it returns the first element in the list. 
Lists are folded from right to left.",
            XForms::Reduce => "The higher order function reduce. Takes a function with two arguments, 
an initial value and a list with at least one term. If there is only one term in the list, 
it returns that. Lists are folded from left to right.",
            XForms::Filter => "The higher order function filter. Takes a predicate with one argument and a list.
Returns a new list whose elements are comprised of members of the original list 
for which the predicate is true.",
            XForms::FilterMap => "The high order function filter-map. Takes a predicate with one argument, 
a function with one argument, and a list. Returns a new list whose elemenets are
comprised of members of the original list for which the predicate is true 
and to which the mapping function has been applied.",
            XForms::Sort => "XForms::Sorts a list from least to greatest values. Takes one list.",
            XForms::RangeList => "Returns a list, taking a beginning (a), an end (b), and optionally a step. 
The default step is one, returning the range (a, b].",
            XForms::Reverse => "Reverses a list. Takes one argument."
        }
    }
}

impl FromStr for XForms {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<XForms,()> {
        match s {
            "map" => Ok(XForms::Map),
            "fold" => Ok(XForms::Fold),
            "foldr" => Ok(XForms::FoldR),
            "reduce" => Ok(XForms::Reduce),
            "filter" => Ok(XForms::Filter),
            "filter-map" => Ok(XForms::FilterMap),
            "sort" => Ok(XForms::Sort),
            "range-list" => Ok(XForms::RangeList),
            "reverse" => Ok(XForms::Reverse),
            _ => Err(())
        }
    }
}

impl fmt::Display for XForms {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            XForms::Map => "map",
            XForms::Fold => "fold",
            XForms::FoldR => "foldr",
            XForms::Reduce => "reduce",
            XForms::Filter => "filter",
            XForms::FilterMap => "filter-map",
            XForms::Sort => "sort",
            XForms::RangeList => "range-list",
            XForms::Reverse => "reverse",
        }));
        Ok(())
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum MatrixOps {
    MakeMatrix, 
    MatrixFromFn,
    MatrixSetRow, 
    MatrixSetCol,
    MatrixAppendRows, 
    MatrixAppendCols,
    MatrixConcatRows, 
    MatrixConcatCols,
    MatrixGetElem, 
    MatrixGetRow, 
    MatrixGetCol,
    Scalar, 
    CrossProd, 
    DotProd, 
    Transpose, 
    Translate,
    Determ, 
    MatrixInv,
    PolygonArea,
}

impl Help for MatrixOps {
    fn help<'a>(&self) -> &'a str {
        match *self {
            MatrixOps::MakeMatrix => "make-matrix takes one argument, a list of lists. Each list represents a row,
and every list must be of equal length.",
            MatrixOps::MatrixFromFn => "Takes a function with at least one argument, and as many lists as there are
arguments. The number of arguments determins how many columns the matrix has, while the number of terms
in the first list determines how many rows the matrix has.",
            MatrixOps::MatrixSetRow => "Set a row in a matrix to a given list. The first argument is the matrix,
the second argument is the row to replace, and the third argument is the new row.",
            MatrixOps::MatrixSetCol => "Set a column in a matrix to a given list. The first argument is the matrix,
the second argument is the colunm to replace, and the third argument is the new column.",
            MatrixOps::MatrixAppendRows => "Append rows to a matrix. Takes two arguments, the first being a matrix,
and the second being a list of lists representing rows.",
            MatrixOps::MatrixAppendCols => "Append columns to a matrix. Takes two arguments, the first being a matrix,
and the second being a list of lists representing columns.",
            MatrixOps::MatrixConcatRows => "Make a matrix from two others with the same number of columns.",
            MatrixOps::MatrixConcatCols => "Make a matrix from two others with the same number of rows.",
            MatrixOps::MatrixGetElem => "Get the element from a matrix in a given row and column. The first term
is the matrix, the second is the row, and the last is the column. Indexed from 1, not 0.",
            MatrixOps::MatrixGetRow => "Get a row from a matrix. Takes one term.",
            MatrixOps::MatrixGetCol => "Get a column from a matrix.Takes one term.",
            MatrixOps::Scalar => "Apply a scalar function to a matrix.",
            MatrixOps::CrossProd => "Get the cross productof two vectors (1 row matrices).",
            MatrixOps::DotProd => "Get the Kronecker product of two matrices.",
            MatrixOps::Transpose => "MatrixOps::Transpose a matrix. Takes one term.",
            MatrixOps::Translate => "MatrixOps::Translate a matrix by given values.",
            MatrixOps::Determ => "Get the determinant of a matrix.",
            MatrixOps::MatrixInv => "Returns the inverse of a matrix.",
            MatrixOps::PolygonArea => "Returns the area of a polygon given as clockwise coordinates in a two column
matrix."
        }
    }
}


impl fmt::Display for MatrixOps {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            MatrixOps::MakeMatrix => "make-matrix",
            MatrixOps::MatrixSetRow => "matrix-set-row",
            MatrixOps::MatrixSetCol => "matrix-set-col",
            MatrixOps::MatrixAppendRows => "matrix-append-rows",
            MatrixOps::MatrixAppendCols => "matrix-append-cols",
            MatrixOps::MatrixConcatRows => "matrix-concat-rows",
            MatrixOps::MatrixConcatCols => "matrix-concat-cols",
            MatrixOps::MatrixGetElem => "matrix-get-elem",
            MatrixOps::MatrixGetRow => "matrix-get-row",
            MatrixOps::MatrixGetCol => "matrix-get-col",
            MatrixOps::Determ => "matrix-det",
            MatrixOps::Scalar => "matrix-scalar",
            MatrixOps::CrossProd => "cross-*",
            MatrixOps::DotProd => "dot-*",
            MatrixOps::Transpose => "matrix-transpose",
            MatrixOps::Translate => "matrix-translate",
            MatrixOps::MatrixInv => "matrix-inv",
            MatrixOps::MatrixFromFn => "matrix-from-fn",
            MatrixOps::PolygonArea => "polygon-area",
        }));
        Ok(())
    }
}

impl FromStr for MatrixOps {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<MatrixOps,()> {
        match s {
            "make-matrix" => Ok(MatrixOps::MakeMatrix),
            "matrix-append-rows" => Ok(MatrixOps::MatrixAppendRows),
            "matrix-append-cols" => Ok(MatrixOps::MatrixAppendCols),
            "matrix-concat-rows" => Ok(MatrixOps::MatrixConcatRows),
            "matrix-concat-cols" => Ok(MatrixOps::MatrixConcatCols),
            "matrix-set-row" => Ok(MatrixOps::MatrixSetRow),
            "matrix-set-col" => Ok(MatrixOps::MatrixSetCol),
            "matrix-get-elem" => Ok(MatrixOps::MatrixGetElem),
            "matrix-get-row" => Ok(MatrixOps::MatrixGetRow),
            "matrix-get-col" => Ok(MatrixOps::MatrixGetCol),
            "matrix-det" => Ok(MatrixOps::Determ),
            "matrix-scalar" => Ok(MatrixOps::Scalar),
            "cross-*" => Ok(MatrixOps::CrossProd),
            "dot-*" => Ok(MatrixOps::DotProd),
            "matrix-inv" => Ok(MatrixOps::MatrixInv),
            "matrix-transpose" => Ok(MatrixOps::Transpose),
            "matrix-translate" => Ok(MatrixOps::Translate),
            "matrix-from-fn" => Ok(MatrixOps::MatrixFromFn),
            "polygon-area" => Ok(MatrixOps::PolygonArea),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum Introspect {
    BoolQ,
    LambdaQ,
    ListQ,
    MatrixQ,
    NumberQ,
    StructQ,
    SymbolQ,
}

impl Help for Introspect {
    fn help<'a>(&self) -> &'a str {
        match *self {
            Introspect::BoolQ   => "Determines if the term is a boolean value.",
            Introspect::LambdaQ => "Determines if the term is a procedure.",
            Introspect::ListQ   => "Determines if the term is a list.",
            Introspect::MatrixQ => "Determines if the term is a matrix.",
            Introspect::NumberQ => "Determines if the term is a number.",
            Introspect::StructQ => "Determines if the term is a struct.",
            Introspect::SymbolQ => "Determines if the term is a symbol.",
        }
    }
}

impl fmt::Display for Introspect {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            Introspect::BoolQ   => "bool?",
            Introspect::ListQ   => "list?",
            Introspect::MatrixQ => "matrix?",
            Introspect::NumberQ => "number?",
            Introspect::LambdaQ => "lambda?",
            Introspect::StructQ => "struct?",
            Introspect::SymbolQ => "symbol?",
        }));
        Ok(())
    }
}

impl FromStr for Introspect {
    #[inline]
    type Err = ();
    fn from_str(s: &str) -> Result<Introspect,()> {
        match s {
            "symbol?" => Ok(Introspect::SymbolQ),
            "number?" => Ok(Introspect::NumberQ),
            "bool?"   => Ok(Introspect::BoolQ),
            "matrix?" => Ok(Introspect::MatrixQ),
            "list?"   => Ok(Introspect::ListQ),
            "lambda?" => Ok(Introspect::LambdaQ),
            "struct?" => Ok(Introspect::StructQ),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum OperatorType {
    Pow,
    Help,
    Graph,
    Table, 
    Quote,
    Define,
    Lambda,
    Logic(Gate),
    TableFromMatrix,
    RecOps(RecordOps),
    Arithmetic(Arith),
    Listings(ListOps),
    Ordering(OrderEq),
    Query(Introspect),
    TransForms(XForms),
    RoundIdent(RoundId),
    MatrixStuff(MatrixOps),
    Transcend(Transcendental),
}

impl fmt::Display for OperatorType {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}", match *self {
            OperatorType::Pow                => "pow".to_string(),
            OperatorType::Help               => "help".to_string(),
            OperatorType::Graph              => "graph".to_string(),
            OperatorType::Table              => "table".to_string(),
            OperatorType::Quote              => "'".to_string(),
            OperatorType::Define             => "define".to_string(),
            OperatorType::Lambda             => "lambda".to_string(),
            OperatorType::Logic(ref x)       => x.to_string(),
            OperatorType::Query(ref x)       => x.to_string(),
            OperatorType::RecOps(ref x)      => x.to_string(),
            OperatorType::Ordering(ref x)    => x.to_string(),
            OperatorType::Listings(ref x)    => x.to_string(),
            OperatorType::Transcend(ref x)   => x.to_string(),
            OperatorType::Arithmetic(ref x)  => x.to_string(),
            OperatorType::RoundIdent(ref x)  => x.to_string(),
            OperatorType::TransForms(ref x)  => x.to_string(),
            OperatorType::MatrixStuff(ref x) => x.to_string(),
            OperatorType::TableFromMatrix    => "table-from-matrix".to_string(),
        }));
        Ok(())
    }
}

impl FromStr for OperatorType {
    #[inline]
    type Err = ();

    fn from_str(s: &str) -> Result<OperatorType,()> {
        match s.parse::<Arith>() {
            Ok(x) => return Ok(OperatorType::Arithmetic(x)),
            Err(()) => { }
        }

        match s.parse::<Transcendental>() {
            Ok(x) => return Ok(OperatorType::Transcend(x)),
            Err(()) => { }
        }

        match s.parse::<OrderEq>() {
            Ok(x) => return Ok(OperatorType::Ordering(x)),
            Err(()) => { }
        }
        
        match s.parse::<Gate>() {
            Ok(x) => return Ok(OperatorType::Logic(x)),
            Err(()) => { }
        }
        
        match s.parse::<ListOps>() {
            Ok(x) => return Ok(OperatorType::Listings(x)),
            Err(()) => { }
        }

        match s.parse::<XForms>() {
            Ok(x) => return Ok(OperatorType::TransForms(x)),
            Err(()) => { }
        }

        match s.parse::<MatrixOps>() {
            Ok(x) => return Ok(OperatorType::MatrixStuff(x)),
            Err(()) => { }
        }

        match s.parse::<RoundId>() {
            Ok(x) => return Ok(OperatorType::RoundIdent(x)),
            Err(()) => { }
        }

        match s.parse::<RecordOps>() {
            Ok(x) => return Ok(OperatorType::RecOps(x)),
            Err(()) => { }
        }

        match s.parse::<Introspect>() {
            Ok(x) => return Ok(OperatorType::Query(x)),
            Err(()) => { }
        }
    
        match s {
            "pow" => Ok(OperatorType::Pow),
            "define" => Ok(OperatorType::Define),
            "lambda" => Ok(OperatorType::Lambda),
            "quote" | "'" => Ok(OperatorType::Quote),
            "table" => Ok(OperatorType::Table),
            "table-from-matrix" => Ok(OperatorType::TableFromMatrix),
            "graph" => Ok(OperatorType::Graph),
            "help" => Ok(OperatorType::Help),
            _ => Err(())
        }
    }
}

impl Help for OperatorType {
    fn help<'a>(&self) -> &'a str {
        match *self {
            OperatorType::Arithmetic(ref x) => x.help(),
            OperatorType::Transcend(ref x) => x.help(),
            OperatorType::Ordering(ref x) => x.help(),
            OperatorType::RoundIdent(ref x) => x.help(),
            OperatorType::Logic(ref x) => x.help(),
            OperatorType::Listings(ref x) => x.help(),
            OperatorType::TransForms(ref x) => x.help(),
            OperatorType::MatrixStuff(ref x) => x.help(),
            OperatorType::RecOps(_) => "No documentation at this time. Future tbd.",
            OperatorType::Pow => "The power operator. Takes two terms, the first being the base and the second is 
the power to which it is raised.",
            OperatorType::Quote => "This isn't correct right now.",
            OperatorType::Define => "Define a variable. You can define it as another variable, 
or the result of a function, or as a function.",
            OperatorType::Lambda => "The Anonymous function. (lambda (arguments) (body)).",
            OperatorType::Table => "The table function. Takes a function with at least one argument,
and as many lists are there are arguments.",
            OperatorType::TableFromMatrix => "Takes a fuction with one fewer argument than the matrix has columns.",
            OperatorType::Query(ref x) => x.help(),
            OperatorType::Graph => "graph takes 8 args, a function, (x, y) orig, width, len, {x,y}-zoom and filename.",
            OperatorType::Help => "The help function has the form (help term1, term2, term3...) and prints out
examples of how operators are used. You can use help for individual operators, 
and you can list operators by group with the following terms: 
arithmetic, logic, and trigonometry (or trig). See also (help use)."
        }
    }
}
