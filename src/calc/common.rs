//! Functions and variables which are used in more than one module.

extern crate types;

use self::types::operator::{Help, OperatorType};
use super::{ArgType, Atom, BadArgType, CalcResult, Symbol, Void};
use std::collections::hashmap::HashMap;

pub fn help(args: &Vec<ArgType>) -> CalcResult {
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
is quite buggy due to the limitations of floating point numbers.";

    let arithmetic_help =
"The arithmetic operators are +, -, *, /, %, and pow.
In general, the arithmetic operators can take 0 terms, or as many terms as you
like.
To see more help, use help with the appropriate arithmetic operator.";

    let trig_help =
"Trigonometric functions. Currently only comprised of sin, cos, and tan. 
Each function only takes one term, or an expression which is evaluated to
a single term.";

    let logic_help =
"The ordering operators are <, <=, =, >=, >. Additionally, you can compose
conditional statements with if. And, Or, and Not are also availalbe.

(> 3 2) ; true
(= 7 3) ; false
(if (> 3 2) (+ 7 3) (/ 10 2)) ; 10";

    let mut help_map: HashMap<String, String> = HashMap::new();

    for (key, val) in ["use", "arithmetic", "trig", "logic"]
        .iter().zip([use_help, arithmetic_help, trig_help, logic_help].iter())
    {
            help_map.insert(key.to_string(), val.to_string());
    }
                                                       

    for arg in args.iter() {
        let term = match arg {
            &Atom(Symbol(ref x)) => x,
            _ => return Err(BadArgType("expected a string".to_string()))
        };
        
        match from_str::<OperatorType>(term.as_slice()) {
            Some(x) => {
                println!("{}", x.help());
                continue
            }
            None => { }
        }

        println!("{}", match help_map.find(term) {
            Some(val) => format!("{}", val),
            None => format!("Help for `{}' is not available", term)
        });
    }

    Ok(Atom(Void))
}
