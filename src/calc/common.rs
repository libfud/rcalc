//! Functions and variables which are used in more than one module.

use super::{ArgType, Atom, Symbol, Void, CalcResult};
use std::collections::hashmap::HashMap;

pub fn help(args: &Vec<ArgType>) -> CalcResult {
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

(/ (- 6 (pow 9 (/ 2)) (+ -4.5 13/2))) ; 2/3

As you can see, you can nest expressions. Not all expressions are evaluated:

(+ 2 (if (> 3 2) (+ 7 3) (* 2))) ; 12

only (> 3 2) and (+ 7 3) are evaluated in that case, and then added to 2.

There are a variety of acceptable input formats for numebers. You can prepend
a negative symbol to the numerator or denominator, use fractions, or numbers
that have a explicit radix (decimal) point. However, mixing explicit radix
points with fractional notation is disallowed, and the preferred method
is either just an integer as a numerator or an explicit fraction. If you
want to express the reciprocal of a number, either input it as 1/n, where n is
a numeric literal, or as (/ n). Input as numbers with an explicit radix 
is quite buggy due to the limitations of IEEE754.";

    let arithmetic_help =
"The arithmetic operators are +, -, *, /, %, and pow.
In general, the arithmetic operators can take 0 terms, or as many terms as you
like. For example, pow acts like a tower of power.

(pow 2 3 4) ; 2417851639229258349412352

is equivalent to (pow 2 81).
To see more help, use help with the appropriate arithmetic operator.
Aadditionally, see (help abs), as I have not classified it yet.";

    let add_help =
"The addition operator. If no terms are supplied, returns the the additive
identity, 0. The examples below are valid expressions:

(+ ) ; 0
(+ 7) ; 7
(+ 2 2) ; 4
(+ 3 4 (/ 25 5) 6) ; 18
(+ -3.5) ; -7/2 ";

    let sub_help = 
"The subtraction operator. Requires at least one term. If only one term is
given, it returns the negation of that term. The following are examples of
valid expressions:

(- 2) ; -2
(- -1.5) ; 3/2
(- 3 2) ; 1
(- 10 3 3) ; 4
(- (+ 2 2) 1) ; 3/";

    let mul_help =
"The multiplication operator. If zero terms are given, returns the
multiplicative identity, one. The following are examples of valid
expressions:

(* ) ; 1
(* -2/3 -3/2) ; 1
(* 2 3 4) ; 24
(* .5 .25) ; 1/8
(* 3/4 100) ; 75";

    let div_help =
"The division operator. Requires at least one term. If only one term is
given, Division by zero is undefined. it returns that term's reciprocal.
Valid examples of expressions:

(/ 2) ; 1/2
(/ 4/5 -1.25) ; -16/25
(/ 24 6 2) ; 2
(/ (pow 2 2 2) 4) ; 4
(/ 144 12 4 3) ; 1";

    let pow_help =
"The exponentiation operator. If supplied no arguments, returns the
multiplcative identity, 1. If only one term is supplied, the implied power is
1. Exponentiation with zero as a base is allowed, but has some notable
behavior:

(pow 0)   ; 0 to any non zero power is 0.
(pow 0 0) ; 1  0^0 is one.
(pow 0 0 0) ; 0  Problem?
(pow 2 2 2 2)  ; 65536
(pow 2 .5)    ; 577/408
(pow 256 1/8) ; 2
(pow 2 -1)    ; 1/2
(pow 8 -2)    ; 1/64";

    let trig_help =
"Trigonometric functions. Currently only comprised of sin, cos, and tan. 
Each function only takes one term, or an expression which is evaluated to
a single term.";

    let sin_help =
"The sine function. Takes one term. If no terms are supplied, it evaluates
zero, which is still zero. Uses radians, not degrees. If you want to
express the angle in degrees, convert with (* degrees pi (/ 180)).

(sin pi)  ; 0
(sin (* 1/2 pi)) ; 1
(sin (/ (* 2 pi) 3)) ; 7800459734587069/9007199254740992";

    let cos_help =
"The cosine function. Takes one term. If no terms are supplied, it evaluates
zero. Uss radians. If you want to express the angle in degrees, convert with
the formula (* degrees pi (/ 180))

(cos 0) ; 1
(cos pi) ; -1";

    let tan_help =
"The tangent function. Takes exactly one term.

(tan pi) ; 0
(tan 0) ; 0";

    let logic_help =
"The ordering operators are <, <=, =, >=, >. Additionally, you can compose
conditional statements with if. And, Or, and Not are also availalbe.

(> 3 2) ; true
(= 7 3) ; false
(if (> 3 2) (+ 7 3) (/ 10 2)) ; 10";

    let condit_help =
"The if statement takes three expressions as arguments: a conditional
statement, a consequent, and an alternative.

(if (> 3 2) (+ 7 3) (/ 10 2)) ; 10
(+ 2 (if (= (pow 2 2 2) (abs -16)) (+ 1 1) (* pi 2)) 2) ; 6";

    let lt_help =
"The ordering operator, <, takes two terms, and returns either true or false.";

    let lte_help =
"The ordering operator, <=, takes two terms, and returns either true or false.";

    let eq_help =
"The ordering operator, =, takes two terms, and returns either true or false.";

    let gte_help =
"The ordering operator, >=, takes two terms, and returns either true or false.";

    let gt_help =
"The ordering operator, >, takes two terms, and returns either true or false.";

    let define_help = 
"Define a variable. You can define it as another variable, or the result of a
function.

(define x 42) ; x = 42
(define y (/ x 7)) ;  y = 6;
(define z (+ (* x y) x)) ; z = 294
(define (f x) (* x 2 (+ x 2))) ; (f 2) is 16, (f 3) is 30
(define (h g x) (* (g x) 3 (+ (g x) 2))) ; (h f 4) is 7200";

    let lambda_help =
"The Anonymous function. (lambda (arguments) (body)).
(reduce (lambda (x y) (+ x y)) 0 '(1 2 3 4 5)) ; 15
(define (h g x) (* (g x) 3 (+ (g x) 2)))
(h (lambda (x) (* x 7)) 4) ; The result is 1080";

    let mut help_map: HashMap<String, String> = HashMap::new();

    for (key, val) in ["help", "use", "arithmetic", "+", "-", "*", "/", "pow", "sin",
                       "cos", "tan", "trig", "<", "<=", "=", ">=", ">", "if", "logic",
                       "define", "lambda"]
        .iter().zip([help_help, use_help, arithmetic_help, add_help, sub_help, mul_help,
                     div_help, pow_help, sin_help, cos_help, tan_help, trig_help,
                     lt_help, lte_help, eq_help, gte_help, gt_help, condit_help, logic_help,
                     define_help, lambda_help].iter())
    {
            help_map.insert(key.to_str(), val.to_str());
    }
                                                       

    for arg in args.iter() {
        let word = match arg {
            &Atom(Symbol(ref x)) => x,
            _ => continue,
        };
        println!("{}", match help_map.find(word) {
            Some(val) => format!("{}", val),
            None => format!("Help for `{}' is not available", word)
        });
/*
        println!("{}", match word.as_slice() {
            "help"              => help_help,
            "use"               => use_help,
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
            "<"                 => lt_help,
            "<="                => lte_help,
            "="                 => eq_help,
            ">="                => gte_help,
            ">"                 => gt_help,
            "if"                => condit_help,
            "logic"             => logic_help,
            "define"            => define_help,
            "lambda"            => lambda_help,
            _                   => "More help is not available at this time."
            }
        )
*/
    }

    Ok(Atom(Void))
}
