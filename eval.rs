//! The eval and parser.

use arithmetic::{add, sub, mul, div, rem, pow, abs};
use trig::{PI, rad, sin, cos, tan};
use stats::avg;
use common::{str_to_f64, ONE_ARG_ONLY, E};
use logic::order;

pub mod arithmetic;
pub mod trig;
pub mod stats;
pub mod common;
pub mod logic;


static BAD_OPERATOR : &'static str = "Improperly placed or missing operator!";
static BAD_EXPR : &'static str = "Poorly formatted expression!";
static BAD_TERM : &'static str = "Poorly formatted term!";

///Wrapper to evaluate a given expression. Checks to make sure that it's a
///valid expression, then does the appropriate action given the operator.
pub fn eval(expr: &str) -> ~str {
    let (operator, terms) = tokenize(expr);
    let answer = match operator.slice_from(0) {
        "+"     => add(terms),
        "-"     => sub(terms),
        "*"     => mul(terms),
        "/"     => div(terms),
        "%"     => rem(terms),
        "pow"   => pow(terms),
        "rad"   => rad(terms),
        "sin"   => sin(terms),
        "cos"   => cos(terms),
        "tan"   => tan(terms),
        "avg"   => avg(terms),
        "abs"   => abs(terms),
        "<" | "<=" | "=" | ">=" | ">" => order(operator, terms),
//        "fac"   => fac(&terms),
//        going to add gamma function instead
        _   => operator
    };

    answer
}

/// Parses an expression and returns a tuple containing its operator and
/// a vector of terms (f64 currently). The operator is found first. If no
/// operator is found before a term, it returns a flag letting eval know
/// the expression is bad. Then, the expression is parsed from the char
/// following the operator, and each character is matched against valid
/// tokens. If the character is not valid, it returns a bad expression.
pub fn tokenize(expr: &str) -> (~str, ~[~str]) {
    let mut terms: Vec<~str> = Vec::new();
    let mut buf = StrBuf::new();
    let mut point_flag = false;
    let mut skip = 0;

    let operator: &str = expr.slice_from(1).words().next().unwrap_or("oops");
    // This essentially forbids terms from touching operators. Which I am
    // 100% fine with.
    match operator {
        "+" | "-" | "*" | "/" | "%"     => { },
        "rad" | "sin" | "cos" | "tan"   => { },
        "pow" | "root" | "avg" | "abs"  => { },
        "<" | "<=" | "=" | ">=" | ">"   => { },
        _   => { return (BAD_OPERATOR.to_owned(), terms.as_slice().to_owned()) }
    }
    let mut op_len = 0;
    let mut last_char = ' ';
    for c in operator.chars().rev() { last_char = c; break; }
        
    for c in expr.chars() {
        op_len += 1;
        if c == last_char { break }
    }

    let mut counter = op_len;
    let mut pi_flag = false;

    for c in expr.slice_from(op_len).chars() {
        if skip != 0 { skip -= 1 }
        else  {
            match c {
                '-'         => {
                    pi_flag = false;
                    if buf.len() == 0 { buf.push_char(c) }
                    else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                },

                '0'..'9'    => { 
                    if pi_flag == false { buf.push_char(c) }
                    else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                },

                'π'         => {
                    if buf.len() != 0 {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                    else { terms.push(PI.to_str().to_owned()) }
                },

                'p'         => {
                    if pi_flag == false && buf.len() == 0 {
                        pi_flag = true;
                    } else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                }

                'i'         => {
                    if pi_flag == true && buf.len() == 0 {
                        terms.push(PI.to_str().to_owned());
                        pi_flag = false;
                    } else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                },

                'e'         => {
                    if buf.len() == 0 { terms.push(E.to_str().to_owned()) }
                    else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                },

                '.'         => { 
                    if point_flag == false {
                        point_flag = true;
                        buf.push_char(c);
                    } else {
                        return (BAD_TERM.to_owned(),terms.as_slice().to_owned())
                    }
                },

                //This potentially signifies the end of a number.
                ' '         => {
                    pi_flag = false;
                    point_flag = false;
                    if buf.len() > 0 {
                        terms.push(buf.to_str().to_owned());
                        buf = "".to_strbuf(); 
                    }
                },
                
                //the start of another expression
                '('         => {
                    //make sure to add the number in the buffer to the vector
                    //if it's present
                    if buf.len() > 0 {
                        terms.push(buf.to_str().to_owned());
                        buf = "".to_strbuf();
                    }

                    let term_len = find_sub_expr_len(expr.slice_from(
                    counter + 1)); // omit the leading parenthesis

                    //evaluating only the slice of the expression holding the
                    //sub expression
                    let term = eval(expr.slice(counter, 
                        counter + term_len + 1).trim_left());

                    //account for failure
                    if term == BAD_EXPR.to_owned() {
                        return (BAD_EXPR.to_owned(),terms.as_slice().to_owned())
                    }
                    
                    terms.push(term);
                    
                    //Account for the subexpression's length
                    counter += term_len;
                    skip = term_len; //an inelegant, ugly hack
                },

                ')'         => {
                    if buf.len() > 0 {
                        terms.push(buf.to_str().to_owned());
                        buf = "".to_strbuf();
                    }
                }

                _           => {
                    return (BAD_EXPR.to_owned(), terms.as_slice().to_owned());
                }
            }
            counter += 1;
        }
    }

    (operator.to_owned(), terms.as_slice().to_owned())
}

/// A helper function to find the length of a sub expression. 
pub fn find_sub_expr_len(expr: &str) -> uint {
    let mut len = 1;
    let mut lparenth = 1;
    let mut rparenth = 0;
    for c in expr.slice_from(1).chars() {
        match c {
            '(' => { lparenth += 1; },
            ')' => { rparenth += 1; },
            _   => { }
        }
        len += 1;
        if rparenth == lparenth { break }
    }
    len 
}

pub fn condit(terms: &[~str]) -> ~str {
    if terms.len() != 3 {
        return "Condition, consequent, and alternative are required".to_owned()
    }

    let condition = eval(terms[0]);
    let (consequent, alternative) = (terms[1], terms[2]);
    if condition != "true" && "condition" != "false" {
        return "Non boolean condition".to_owned()
    }

    if condition == true {
        return eval(consequent)
    } else {
        return eval(alternative)
    }
}
