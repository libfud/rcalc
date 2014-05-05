#![crate_id = "rcalc"]
#![crate_type = "bin"]

//! Polish notation calculator.

use std::io;
use arithmetic::{add, sub, mul, div, rem, pow};
use trig::{rad, sin, cos, tan};
use stats::avg;

pub mod arithmetic;
pub mod trig;
pub mod stats;

static PI: f64 = 3.141592653589793;
//most accurate representation of pi possible in f64

static BAD_OPERATOR : &'static str = "Improperly placed or missing operator!";
static BAD_EXPR : &'static str = "Poorly formatted expression!";
static BAD_TERM : &'static str = "Poorly formatted term!";
static ONE_ARG_ONLY : &'static str = "This function only takes one argument!";
static DESPAIR: &'static str = "Laundry day is a very dangerous day.";

///Wrapper to evaluate a given expression. Checks to make sure that it's a
///valid expression, then does the appropriate action given the operator.
pub fn eval(expr: &str) -> ~str {
    if validate(expr) == false { return BAD_EXPR.to_owned() }
    let (operator, terms) = tokenize(expr);
    let terms_slice = terms.as_slice();
    let answer = match operator.slice_from(0) {
        "+"     => add(terms_slice),
        "-"     => sub(terms_slice),
        "*"     => mul(terms_slice),
        "/"     => div(terms_slice),
        "%"     => rem(terms_slice),
        "pow"   => pow(terms_slice),
        "rad"   => rad(terms_slice),
        "sin"   => sin(terms_slice),
        "cos"   => cos(terms_slice),
        "tan"   => tan(terms_slice),
        "avg"   => avg(terms_slice),
//        "fac"   => fac(&terms),
//        going to add gamma function instead
        _   => operator
    };

    answer
}

/// Checks an expression. The expression must have an equal number of 
/// parentheses, and for each set of parentheses, must be an operator.
/// This does not check for the number of terms for each expression, as
/// some expressions may have zero terms. Additionally, it can't check
/// for bad tokens, instead passing the buck to tokenize.
pub fn validate(expr: &str) -> bool {
    if expr.len() <= 1 { return false }
    let mut lparenth = 0;
    let mut rparenth = 0;
    let mut operators = 0;
    let mut char_counter = 0;
    for c in expr.chars() {
        match c {
            '0'..'9' | ' ' | 'π' => { },
            '(' => { lparenth += 1 },
            ')' => { 
                if lparenth - 1 == rparenth && char_counter + 1 < expr.len() {
                    return false
                } else {
                    rparenth += 1;
                }
            },
            '+' | '-' | '*' | '/' | '%' => { operators += 1 },
            _   => { }  //can't catch bad letters here now since we have to
                        //also look for operators which are words
        }
        char_counter += 1;
    }
    if lparenth != rparenth || lparenth < 1 { return false }
    for w in expr.words() {
        let word = match w.slice_to(1) {
            "(" => w.slice_from(1),
            _   => w
        };
        match word {
            "pow" | "root"  => { operators += 1 },
            "avg"           => { operators += 1 },
            "sin" | "cos" | "tan" | "rad" => { operators += 1 },
            _   => { }  //do nothing still because this kind of filter is
                        //impossible
        }
    }
    if lparenth > operators { return false } //- is an operator and a sign

    true
}

/// Parses an expression and returns a tuple containing its operator and
/// a vector of terms (f64 currently). The operator is found first. If no
/// operator is found before a term, it returns a flag letting eval know
/// the expression is bad. Then, the expression is parsed from the char
/// following the operator, and each character is matched against valid
/// tokens. If the character is not valid, it returns a bad expression.
pub fn tokenize(expr: &str) -> (~str, Vec<f64>) {
    let mut terms = Vec::new();
    let mut buf = StrBuf::new();
    let mut point_flag = false;
    let mut skip = 0;

    let operator: &str = expr.slice_from(1).words().next().unwrap_or("oops");
    // This essentially forbids terms from touching operators. Which I am
    // 100% fine with.
    match operator {
        "+" | "-" | "*" | "/" | "%"    => { },
        "rad" | "sin" | "cos" | "tan"  => { },
        "pow" | "root" | "avg" => { },
        _   => { return (BAD_OPERATOR.to_owned(), terms) }
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
                    else { return (BAD_TERM.to_owned(), terms) }
                },

                '0'..'9'    => { 
                    if pi_flag == false { buf.push_char(c) }
                    else { return (BAD_TERM.to_owned(), terms) }
                },

                'π'         => {
                    if buf.len() != 0 { return (BAD_TERM.to_owned(), terms) }
                    else { terms.push(PI) }
                },

                'p'         => {
                    if pi_flag == false && buf.len() == 0 {
                        pi_flag = true;
                    } else { return (BAD_TERM.to_owned(), terms) }
                }

                'i'         => {
                    if pi_flag == true && buf.len() == 0 {
                        terms.push(PI);
                        pi_flag = false;
                    } else { return (BAD_TERM.to_owned(), terms) }
                },

                '.'         => { 
                    if point_flag == false {
                        point_flag = true;
                        //prepend a zero in preparation for the cast
                        if buf.len() == 0 { buf.push_char('0') }
                        buf.push_char(c);
                    } else {
                        return ("Improperly formatted term!".to_owned(), terms)
                    }
                },

                //This potentially signifies the end of a number.
                ' '         => {
                    pi_flag = false;
                    point_flag = false;
                    if buf.len() > 0 {
                        match from_str::<f64>(buf.to_str()) {
                            Some(num)   => terms.push(num),
                            _           => {
                                println!("{}", DESPAIR);
                                return (operator.to_owned(), terms)
                            }
                        }
                        buf = "".to_strbuf(); 
                    }
                },
                
                //the start of another expression
                '('         => {
                    //make sure to add the number in the buffer to the vector
                    //if it's present
                    if buf.len() > 0 {
                        match from_str::<f64>(buf.to_str()) {
                            Some(num)   => { terms.push(num) },
                            _           => {
                                println!("{}", DESPAIR);
                                return (BAD_OPERATOR.to_owned(), terms);
                            }
                        }
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
                        return (BAD_EXPR.to_owned(), terms)
                    }
                    
                    match from_str::<f64>(term) {
                        Some(num)   => { terms.push(num) },
                        _           => {
                            println!("{}", DESPAIR);
                            return (operator.to_owned(), terms)
                        }
                    }
                    
                    //Account for the subexpression's length
                    counter += term_len;
                    skip = term_len; //an inelegant, ugly hack
                },

                ')'         => {
                    if buf.len() > 0 {
                        match from_str::<f64>(buf.to_str()) {
                            Some(num)   => { terms.push(num) },
                            _           => {
                                println!("{}", DESPAIR);
                                return (BAD_OPERATOR.to_owned(), terms);
                            }
                        }
                        buf = "".to_strbuf();
                    }
                }

                _           => {
                    return (BAD_EXPR.to_owned(), terms);
                }
            }
            counter += 1;
        }
    }

    (operator.to_owned(), terms)
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

/// Returns the absolute value of the number.
pub fn abs(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    if terms[0] > 0.0 { return terms[0].to_str().to_owned() }
    
    return sub(terms)
}

fn main() {
    let mut reader = io::stdin();
    let mut expr;

    loop {
        expr = reader.read_line().ok().unwrap_or("exit".to_owned());
        if expr.trim() == "exit".to_owned() { break }
        let output = eval(expr.trim());
        println!("{}", output);
    }
}
