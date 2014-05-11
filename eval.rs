//! The eval and parser.

use arithmetic::{add, sub, mul, div, rem, pow, abs};
use trig::{PI, rad, deg, sin, cos, tan};
use stats::avg;
use common::{E};
use logic::order;

pub mod arithmetic;
pub mod trig;
pub mod stats;
pub mod common;
pub mod logic;

static BAD_EXPR : &'static str = "Poorly formatted expression!";
static BAD_TERM : &'static str = "Poorly formatted term!";

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

/// Parses an expression and returns a tuple containing its operator and
/// a vector of terms (f64 currently). The operator is found first. If no
/// operator is found before a term, it returns a flag letting eval know
/// the expression is bad. Then, the expression is parsed from the char
/// following the operator, and each character is matched against valid
/// tokens. If the character is not valid, it returns a bad expression.
pub fn tokenize(expr: &str) -> Result<~[~str], &str> {
    let mut terms: Vec<~str> = Vec::new();
    let mut buf = StrBuf::new();
    let mut skip = 0;

    let mut counter = 0;

    let inspect_string = |word_buffer: &str| -> (bool, ~str) {
        assert!(word_buffer.len() > 0);

        match word_buffer {
            "π" | "pi"  => { return (true, PI.to_str().to_owned()) },
            "e"         => { return (true, E.to_str().to_owned()) },
            "true"      => { return (true, "true".to_owned()) },
            "false"     => { return (true, "false".to_owned()) },
            _           => { }, //it's likely a numeric literal
        }

        let mut negative_sign_counter = 0;
        let mut radix_point_counter = 0;
        let mut fraction_counter = 0;

        if word_buffer.starts_with("/") || word_buffer.ends_with("/") {
            return (false, BAD_TERM.to_owned())
        }

        for c in word_buffer.chars() {
            match c {
                '0'..'9'    => { }, //do nothing
                '-'         => { negative_sign_counter += 1 },
                '.'         => { radix_point_counter += 1 },
                '/'         => { fraction_counter += 1 }
                _           => { return (false, BAD_TERM.to_owned()) },
            }
        }

        match (fraction_counter, radix_point_counter, negative_sign_counter) {
            (0, 0, 0) | (1, 0, 0) | (0, 1, 0)   => { return (true, word_buffer.to_owned()) },

            (0, 0, 1) | (0, 1, 1) | (1, 0, 1)   => {
                if word_buffer.slice_to(1) == "-" {
                    (true, word_buffer.to_owned())
                } else {
                    (false, BAD_TERM.to_owned())
                }
            },

            _   => { (false, BAD_TERM.to_owned()) }
        }
    };

    for c in expr.chars() {
        if skip != 0 { skip -= 1 }
        else  {
            match c {
                //This potentially signifies the end of a number.
                ' '         => {
                    if buf.len() > 0 {
                        let (valid_token, token) = inspect_string(buf.to_str());
                        if valid_token == false {
                            return Err(BAD_EXPR)
                        }

                        terms.push(token);
                        buf = "".to_strbuf();
                    }
                },
                
                //the start of another expression
                '('         => {
                    //make sure to add the number in the buffer to the vector
                    //if it's present
                    if buf.len() > 0 {
                        let (valid_token, token) = inspect_string(buf.to_str());
                        if valid_token == false { 
                            return Err(BAD_EXPR)
                        }

                        terms.push(token);
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
                        return Err(BAD_EXPR)
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

                _           => { buf.push_char(c) }
            }
            counter += 1;
        }
    }

    Ok(terms.as_slice().to_owned())
}

/// Evaluates conditional statements. Currently only supports one condition,
/// one consequent, and one alternative.
pub fn condit(terms: &[~str]) -> ~str {

    if terms.len() != 3 {
        return "Condition, consequent, and alternative are required".to_owned()
    }

    if terms[0] == "true".to_owned() { //terms[0] being the condition
        terms[1].to_owned() //consequent
    } else if terms[0] == "false".to_owned() {
        terms[2].to_owned() //alternative
    } else {
        "Non boolean condition".to_owned()
    }
}

///Wrapper to evaluate a given expression. Checks to make sure that it's a
///valid expression, then does the appropriate action given the operator.
pub fn eval(expr: &str) -> ~str {
    if expr.slice_to(1) != "(" || expr.slice_from(expr.len() - 1) != ")" {
        return BAD_EXPR.to_owned()
    }

    let operator = expr.slice_from(1).words().next().unwrap_or("oops");
    let op_len = operator.len() + 1;

    let terms = match tokenize(expr.slice_from(op_len+1)) {
        Ok(good_terms)  => good_terms,
        Err(msg)        => return msg.to_owned()
    };

    let answer = match operator {
        "+"     => add(terms),
        "-"     => sub(terms),
        "*"     => mul(terms),
        "/"     => div(terms),
        "%"     => rem(terms),
        "pow"   => pow(terms),
        "rad"   => rad(terms),
        "deg"   => deg(terms),
        "sin"   => sin(terms),
        "cos"   => cos(terms),
        "tan"   => tan(terms),
        "avg"   => avg(terms),
        "abs"   => abs(terms),
        "<" | "<=" | "=" | ">=" | ">" =>
            order(operator, terms),
        "if"    => condit(terms),
//        "fac"   => fac(&terms),
//        going to add gamma function instead
        _   => operator.to_owned()
    };

    answer
}
