#![crate_id = "rcalc"]
#![crate_type = "bin"]

//! Polish notation calculator.

use std::io;
use std::num::Float;
use arithmetic::{add, sub, mul, div, rem};

pub mod arithmetic;

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
    let termslice = terms.as_slice();
    let answer = match operator.slice_from(0) {
        "+"     => add(termslice),
        "-"     => sub(termslice),
        "*"     => mul(termslice),
        "/"     => div(termslice),
        "%"     => rem(termslice),
        "pow"   => pow(termslice),
        "rad"   => rad(termslice),
        "sin"   => sin(termslice),
        "cos"   => cos(termslice),
        "tan"   => tan(termslice),
//        "fac"   => fac(&terms),
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
            "pow" | "sin" | "cos" | "tan" | "rad" => { operators += 1 },
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
        "pow" | "sin" | "cos" | "tan"  => { },
        "rad" => { },
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

/*
/// Adds the numbers in a vector. If there are zero terms, it returns 0.
pub fn add(terms: &[f64]) -> ~str {
    let  mut total = 0f64;
    for term in terms.iter() {
        total += *term;
    }

    total.to_str().to_owned()
}

/// Subtracts the numbers in a vector. At least one term is required. If
/// there is only one term, it returns the negative value of that term.
/// Otherwise, it starts subtracting from the left element. IE, if you
/// have an expression (- 10 3 2), 3 is subtracted from 10, and 2 is
/// subtracted from that value.
pub fn sub(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Subtraction requires at least one term!");
        return BAD_EXPR.to_owned()
    } 
    if terms.len() == 1 {
        let difference = 0f64 - terms[0];
        //negative val of first term
        return difference.to_str().to_owned()
    }
    let mut difference = terms[0];
    for term in terms.slice_from(1).iter(){ difference -= *term }

    difference.to_str().to_owned()
}

/// Multiplies the numbers in a vector. Returns 1 for no terms. Otherwise
/// it returns the product of all numbers in a vector.
pub fn mul(terms: &[f64]) -> ~str {
    let mut product = 1f64;
    for term in terms.iter() { 
        match *term {
            0.0 => { return "0".to_owned() }
            _   => { product *= *term } 
        }
    }
    
    product.to_str().to_owned()
}

/// Divides the numbers in a vector. Requires at least one term. If there is
/// only one term, it returns its inverse. Otherwise, it returns the quotient
/// of the first term by the following terms. For example, (/ 12 2 3) will
/// be evaluated as 12 / 2 (6), divided by 3 ( 6 / 3 = 2)
pub fn div(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Division requires at least one term!");
        return BAD_EXPR.to_owned()
    }
    if terms.len() == 1 { 
        match terms[0] {
            0.0  => { return DIV_BY_ZERO.to_owned() }
            _    => { return (1f64 / terms[0]).to_str().to_owned(); }
        }
    }
    let mut quotient = terms[0];
    if quotient == 0.0 || quotient == -0.0 { return DIV_BY_ZERO.to_owned() }
    for term in terms.slice_from(1).iter() { 
        match *term {
            0.0 => { return DIV_BY_ZERO.to_owned() }
            _   => { quotient /= *term }
        }
    }

    quotient.to_str().to_owned()
}

/// Returns the remainder from integer division. Casts the terms to integers.
/// Requires at least one term; if there is only one term, 1 is returned.
/// Otherwise, it functions similarly to division and subtraction.
pub fn rem(terms: &[f64]) -> ~str {
    if terms.len() < 1 {
        println!("Modulus operations require at least two terms!");
        return BAD_EXPR.to_owned()
    }
    if terms.len() == 1 { 
        match terms[0] {
            0.0 => { return DIV_BY_ZERO.to_owned() }
            _   => { return "1".to_owned() } // 1 % anything = 1
        }
    }
    let mut remainder = terms[0] as int;
    if terms[0] == 0.0 { return .to_owned() }
    for term in terms.slice_from(1).iter() { 
        match *term {
            0.0 => { return DIV_BY_ZERO.to_owned() },
            _   => { remainder %= *term as int }
        }
    }

    remainder.to_str().to_owned()
}
*/

/// Pow raises a number to a power - if there are more than one terms,
/// it behaves like a tower of power. It uses the identity function
/// for no terms and for the case of (pow 0 0). In the case of (pow 0 0 0),
/// this will actually evaluate to 0, and (pow 0 0 0 0) will evaluate
/// to one again. This behavior is periodic. Towers are evaluated recursively.
/// If only one number is passed, the number is returned, unless it is zero,
/// which returns zero.
pub fn pow(terms: &[f64]) -> ~str {
    if terms.len() == 0 { return "1".to_owned() }
    if terms.len() == 1 { 
        if terms.as_slice()[0] != 0.0 { 
            return terms[0].to_str().to_owned()
        }
        else { return "0".to_owned() }
    }
    let base = terms[0];
    let mut exponent : f64;
    if terms.len() == 2 {
        exponent = terms.as_slice()[1];
    } else {
        let temp_exponent = pow(terms.slice_from(1));
        match from_str::<f64>(temp_exponent) {
            Some(good_value)    => { exponent = good_value },
            _                   => { return DESPAIR.to_owned() }
        }
    }
    if base == 0.0 && exponent == 0.0 { return "1".to_owned() }
    else if base == 0.0 { return "0".to_owned() }

    let mut rootx = 1.0;
  
    let mut recip_flag = false;
    if exponent < 0.0 { 
        recip_flag = true;
        exponent = exponent.abs();
    }

    let index = exponent - exponent.floor();
    if index > 0.0 {
        match from_str::<f64>(root_wrapper(&[base, index.recip()])) {
            Some(num)   => { rootx = num },
            _           => { return DESPAIR.to_owned() }
        }
    }

    let mut product = 1f64;
    for _ in range(0, exponent.floor() as int) { product *= base; }
    product *= rootx;

    if recip_flag == true { product = 1.0 / product }

    product.to_str().to_owned()
}

/// Root finds a number which when raised to a power equal to the index is
/// equal to the radicand. It requires two arguments: the index and a
/// radicand. 
pub fn root_wrapper(terms: &[f64]) -> ~str {
    if terms.len() != 2 { 
        return "A radicand and index, only, are required.".to_owned()
    }
    let (radicand, index) = (terms[0], terms[1]);

    if index == 0.0 { return "1".to_owned() } //handles (root 0 0)
    if radicand == 0.0 { return "0".to_owned() }
    if index % 2.0 == 0.0 && radicand < 0.0 {
        return "I can't handle this complexity!".to_owned()
    }

    if index != 2.0 {
        println!("Only square roots are possible for now.");
        return BAD_EXPR.to_owned()
    }

    let denominator = match index.floor() < index {
        false   => 0.0, //this will be passed to pow as the power
        true    => index - index.floor()
    };

    let mut factor_str = "1".to_owned();
    if denominator > 0.001 {
        factor_str = pow(&[radicand, denominator.recip()]);
    }
    let mut factor: f64;
    match from_str::<f64>(factor_str) {
        Some(num)   =>  { factor = num },
        _           =>  { return DESPAIR.to_owned() }
    }
    
    let numerator = index.floor();
    let guess = 1.0;
    let root_of_radicand = root(guess, radicand, numerator);
    if root_of_radicand == -0.0 { return DESPAIR.to_owned() }

    let answer = root_of_radicand * factor;

    answer.to_str().to_owned()
}

/// This is the means to which the root function can attain recursion.
/// Compares the absolute value of the difference of the guess raised to the
/// power and the radicand to a tolerance. If it's within tolerance, that
/// number is returned. Otherwise, it uses the average
pub fn root(guess: f64, radicand: f64, index: f64)  -> f64 {
    let tolerance = match index {
        2.0 => 0.00001,
        _   => 1.0
    };
    let mut guess_to_pow: f64;
    match from_str::<f64>(pow(&[guess, index])) {
        Some(num)   => { guess_to_pow = num },
        _           => { return -0.0 }
    }
    if (guess_to_pow - radicand).abs() < tolerance {
        return guess
    }
    let new_guess = (guess + radicand / guess) / 2.0;
    root(new_guess, radicand, index)
}

/// Avg function returns the arithmetic mean of the terms.
pub fn avg(terms: &[f64]) -> ~str {
    if terms.len() < 1 { 
        return "This function requires at least one term.".to_owned()
    }
    let average = match from_str::<f64>(add(terms)) {
        Some(num)   => (num / terms.len() as f64).to_str(),
        _           => DESPAIR.to_owned()
    };

    average
}

pub fn abs(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    if terms[0] > 0.0 { return terms[0].to_str().to_owned() }
    
    return sub(terms)
}

/// Rad converts degrees to radians. Its use is not recommended and it is
/// preferred for other functions to use radsians in the first place. In fact,
/// all other trigonometric functions assume that radians are being used, so
/// if the user wants to use degrees, he will have to use the rad function
/// to convert.
pub fn rad(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let radians = terms[0] * PI / 180.0;

    radians.to_str().to_owned()
}

/// The sin function. Takes either zero or one terms. For no terms,
/// 0 is returned.
pub fn sin(terms: &[f64]) -> ~str {
    if terms.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    if terms.len() == 0 { return "0".to_owned() }
    let answer = terms[0].sin();

    answer.to_str().to_owned()
}

/// The cos function. Takes either zero or one terms. For no terms, 1 is 
/// returned.
pub fn cos(terms: &[f64]) -> ~str {
    if terms.len() > 1 { return ONE_ARG_ONLY.to_owned() }
    if terms.len() == 0 { return "0".to_owned() }
    let answer = terms[0].cos();

    answer.to_str().to_owned()
}

/// The tan function. Takes exactly one argument.
pub fn tan(terms: &[f64]) -> ~str {
    if terms.len() != 1 { return ONE_ARG_ONLY.to_owned() }
    let answer = terms[0].tan();

    answer.to_str().to_owned()
}

/*
pub fn fac(terms: &Vec<f64>) -> ~str {
    if terms.len() != 1 {
        println!("Factorials only take one term!");
        return BAD_EXPR.to_owned()
    }
    if terms.as_slice()[0]j
*/
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
