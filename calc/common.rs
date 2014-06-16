//! Functions and variables which are used in more than one module.

extern crate num;

use super::{Evaluate, CalcResult};
use super::literal::{ Void};
use self::num::rational::{BigRational, Ratio};
use std::num;

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
static PI: &'static str = "3126535/995207";

pub fn help(args: &Vec<Box<Evaluate>>) -> CalcResult {
    let help_help =
"The help function has the form (help term1, term2, term3...) and prints out
examples of how operators are used. You can use help for individual operators
for individual operators, and you can list operators by group with the 
following terms: arithmetic, logic, trigonometry (or trig), and statistics
(or stats). See also (help use). An example of usage is

(+ 2 2)";

    let use_help =
"rcalc is an arbitrary precision polish notation (lisp style) calculator.
It requires that expressions are wrapped in parentheses ( \"( and \") ).
The operator goes first in any expression, followed by its terms.
For an  example, the following returns -3/2:

(/ (- 6 (pow 9 (/ 2)) (+ -4.5 13/2)))

As you can see, you can nest expressions. Not all expressions are evaluated:

(+ 2 (if (> 3 2) (+ 7 3) (* 2)))

only (> 3 2) and (+ 7 3) are evaluated in that case, and then added to 2.

There are a variety of acceptable input formats for numebers. You can prepend
a negative symbol to the numerator or denominator, use fractions, or numbers
that have a explicit radix (decimal) point. However, mixing explicit radix
points with fractional notation is disallowed, and the preferred method
is either just an integer as a numerator or an explicit fraction. If you
want to express the reciprocal of a number, either input it as 1/n, where n is
a numeric literal, or as (/ n). Input as numbers with an explicit radix 
is quite buggy due to the limitations of IEEE754.";

    let abs_help =
"The absolute function. Takes only one term, and returns its absolute value.

(abs -5.5) -> 11/2
(abs 17/7) -> 17/7";
    
    let arithmetic_help =
"The arithmetic operators are +, -, *, /, %, and pow.
In general, the arithmetic operators can take 0 terms, or as many terms as you
like. For example, pow acts like a tower of power.

(pow 2 3 4)

is equivalent to (pow 2 81).
To see more help, use help with the appropriate arithmetic operator.
Aadditionally, see (help abs), as I have not classified it yet.";

    let stats_help =
"Statistical functions. Currently only comprised of avg, which returns the
arithmetic mean of a list of terms.

(avg 42) -> 42/1
(avg 7.5 12 -13.25 4/5) -> 161/80";

    let add_help =
"The addition operator. If no terms are supplied, returns the the additive
identity, 0. The examples below are valid expressions:

(+ ) -> 0/1
(+ 7) -> 7/1
(+ 2 2) -> 4/1
(+ 3 4 (/ 25 5) 6) -> 18/1
(+ -3.5) -> -7/2 ";

    let sub_help = 
"The subtraction operator. Requires at least one term. If only one term is
given, it returns the negation of that term. The following are examples of
valid expressions:

(- 2) -> -2/1
(- -1.5) -> 3/2
(- 3 2) -> 1/1
(- 10 3 3) -> 4/1
(- (+ 2 2) 1) -> 3/1";

    let mul_help =
"The multiplication operator. If zero terms are given, returns the
multiplicative identity, one. The following are examples of valid
expressions:

(* ) -> 1/1
(* -2/3 -3/2) -> 1/1
(* 2 3 4) -> 24/1
(* .5 .25) -> 1/8
(* 0.75 100) -> 750";

    let div_help =
"The division operator. Requires at least one term. If only one term is
given, Division by zero is undefined. it returns that term's reciprocal.
Valid examples of expressions:

(/ 2) -> 1/2
(/ 4/5 -1.25) -> -16/25
(/ 24 6 2) -> 2
(/ (pow 2 2 2) 4) -> 4
(/ 144 12 4 3) -> 1";

    let pow_help =
"The exponentiation operator. If supplied no arguments, returns the
multiplcative identity, 1. If only one term is supplied, the implied power is
1. Exponentiation with zero as a base is allowed, but has some notable
behavior:

(pow 0) -> 0/1     | 0 to any non zero power is 0.
(pow 0 0) -> 1/1   | 0^0 is one.
(pow 0 0 0) -> 0/1 | Problem?

If you are asking what's going on with 0, it's because pow is evaluated
recursively from left to right: if more than two terms are supplied, it
essentially evaluates from the rightmost two terms, treating the very
last as the power and the second to last as the base.

(pow 0 0 0) -> (pow 0 (pow 0 0)) -> (pow 0 1) -> 0/1

That's all I have to say on that matter. Below are valid expressions:

(pow 2 2 2 2) -> 65536/1
(pow 2 3 4)   -> 24178851639229258349412352/1
(pow 2 .5)    -> 577/408
(pow 27 1/3)  -> 3/1
(pow 256 1/8) -> 2/1
(pow 2 -1)    -> 1/2
(pow 8 -2)    -> 1/64";

    let trig_help =
"Trigonometric functions. Currently only comprised of sin, cos, and tan. 
Each function only takes one term, or an expression which is evaluated to
a single term.";

    let sin_help =
"The sine function. Takes one term. If no terms are supplied, it evaluates
zero, which is still zero. Uses radians, not degrees. If you want to
express the angle in degrees, convert with (* degrees pi (/ 180)).

(sin pi)    -> 0
(sin (* 1/2 pi)) -> 1
(sin (/ (* 2 pi) 3)) -> 0.866025";

    let cos_help =
"The cosine function. Takes one term. If no terms are supplied, it evaluates
zero. Uss radians. If you want to express the angle in degrees, convert with
the formula (* degrees pi (/ 180))

(cod ) -> 1/1
(cos pi) -> -1";

    let tan_help =
"The tangent function. Takes exactly one term.

(tan 1) -> 1.55
(tan pi) -> 0
(tan 0) -> 0";

    let avg_help =
"Returns the arithmetic mean of a set of numbers. Requires at least one term.

(avg 2 3 4) -> 3/1
(avg 12.25 -7/3 -14) -> -49/36
(avg 42 6 7 13 9 9 9) -> 95/7";

    let logic_help =
"The ordering operators are <, <=, =, >=, >. Additionally, you can compose
conditional statements with if. And, Or, and Not are also availalbe.

(> 3 2) -> true
(= 7 3) -> false
(if (> 3 2) (+ 7 3) (/ 10 2)) -> 10/1";

    let condit_help =
"The if statement takes three expressions as arguments: a conditional
statement, a consequent, and an alternative.

(if (> 3 2) (+ 7 3) (/ 10 2)) -> 10/1
(+ 2 (if (= (pow 2 2 2) (abs -16)) (+ 1 1) (* pi 2)) 2) -> 6/1";

    let lt_help =
"The ordering operator, <, takes two terms, and returns either true or false.

(< 3 2) -> true
(< 3 pi) -> false";

    let lte_help =
"The ordering operator, <=, takes two terms, and returns either true or false.

(<= 7 6) -> true
(<= 6/3 3/2) -> false";

    let eq_help =
"The ordering operator, =, takes two terms, and returns either true or false.

(= 13 13) -> true
(= pi e) -> false";

    let gte_help =
"The ordering operator, >=, takes two terms, and returns either true or false.

(>= 7 6) -> true
(>= pi 13/2) -> false";

    let gt_help =
"The ordering operator, >, takes two terms, and returns either true or false.

(> 9 5/4) -> true
(> e pi) -> false";

    let define_help = 
"Define a variable. You can define it as another variable, or the result of a
function.

(define x 42)
(define y (/ x 7))
(define z (+ (* x y) x))";

let defun_help = 
"Define a function. Place the function between pipes.

(defun |f (x) (+ x 2)|)
(defun |g (x y) (* x y)|)
(defun |h (x y z) (/ (* x (+ x 1) (* 2 (+ x 1))) 6)";

    let list: Vec<String> = args.iter().map(|x| x.to_symbol()).collect();

    if list.len() == 0 {
        println!("{}", help_help)
        return Ok(Void)
    }

    for word in list.iter() {
        println!("{}", match word.as_slice() {
            "help"              => help_help,
            "use"               => use_help,
            "abs"               => abs_help,
            "arithmetic"        => arithmetic_help,
            "+"|"add"           => add_help,
            "-"|"subtraction"   => sub_help,
            "*"|"multiply"      => mul_help,
            "/"|"division"      => div_help,
            "pow"|"power"       => pow_help,
            "sin"|"sine"        => sin_help,
            "cos"|"cosine"      => cos_help,
            "tan"|"tangent"     => tan_help,
            "trig"              => trig_help,
            "stats"             => stats_help,
            "avg"               => avg_help,
            "<"                 => lt_help,
            "<="                => lte_help,
            "="                 => eq_help,
            ">="                => gte_help,
            ">"                 => gt_help,
            "if"                => condit_help,
            "logic"             => logic_help,
            "define"            => define_help,
            "defun"             => defun_help,
            _                   => "More help is not available at this time."
            }
        )
    }

    return Ok(Void)
}

/// Enumeration of ways to write numbers.
pub enum NumEncoding {
    Fraction,
    NonFraction,
    Invalid
}

/// Converts a string into a bigrational.
pub fn str_to_rational(word: &str) -> Result<BigRational, String> {

    let number_type = get_num_encoding(word);
    match number_type {
        Fraction    => Ok(from_str::<BigRational>(word).unwrap()),

        NonFraction => {
            let floated =  from_str::<f64>(word).unwrap();
            Ok(Ratio::from_float(floated).unwrap())
        },

        Invalid     => Err("Unexpected return type!".to_str())
    }
}

/// Determines if a number is represented as a fraction or not.
pub fn get_num_encoding(num_str: &str) -> NumEncoding {
    if num_str.slice_to(1) == "/" || num_str.slice_to(num_str.len() -1) == "/" { 
            return Invalid
    }

    let (divisors, radices) = num_str.chars().fold((0, 0), |(mut x, mut y), c| {
        if c == '/' {
            x += 1
        } else if c == '.' {
            y += 1
        }
        (x, y)
    });

    match (divisors, radices) {
        (0, 0) | (0, 1) => NonFraction,
        (1, 0)          => Fraction,
        _   => Invalid
    }
}

/// Reduces a rational to a value in f64s range of accuracy, then converts it to an f64
/// and then returns that value for use in trigonometric functions.
pub fn rational_to_f64_trig(bigrational_orig: &BigRational) -> f64 {
    let mut bigrational = bigrational_orig.clone();

    let two: BigRational = num::one::<BigRational>() + num::one();
    let twopi = from_str::<BigRational>(PI).unwrap().mul(&two);
    let jump = from_str::<BigRational>("100000000000/1").unwrap() * twopi;
    let upper = from_str::<BigRational>("9007199254740992/1").unwrap(); //2^53
    let lower = from_str::<BigRational>("-9007199254740992/1").unwrap();

    if bigrational > upper {
        while bigrational > upper {
            bigrational = bigrational - jump;
        }
    } else if bigrational < lower {
        while bigrational < lower {
            bigrational = bigrational + jump;
        }
    }

    let numer = bigrational.numer().to_f64().unwrap();
    let denom = bigrational.denom().to_f64().unwrap();

    numer / denom
}
