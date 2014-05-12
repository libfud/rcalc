// Read

static BAD_EXPR: &'static str = "Poorly formatted expression";
static OPERATORS: &'static [&'static str] = &'static [
    "+", "-", "*", "/", "%", "pow",
    "rad", "deg", "sin", "cos", "tan",
    "avg", "abs",
    "<", "<=", "=", ">=", ">", "if"
];

/// Enumeration of kinds of valid tokens.
#[deriving(Clone)]
pub enum Token {
    Operator,
    Boolean,
    Numeric,
    Lparen,
    Rparen,
    Invalid,
}

/// Examines an input string to see if it is properly formatted, and generates
/// a set of tokens for use by parse.
pub fn lexer(expr: &str) -> Result<~[(Token, ~str)], &str> {

    let mut lparens = 0;
    let mut rparens = 0;

    //Check to see if the first and last characters are lparen and rparen.
    if expr.slice_to(1) != "(" || expr.slice_from(expr.len() -1) != ")" {
        return Err(BAD_EXPR)
    }

    // Count the parentheses to ensure there are an even amount.
    for c in expr.chars() {
        if c == '(' {
            // if there are as many rparens as lparens and another lparen is 
            // encountered, this signifies a split expression. For example,
            // (+ 2 2)(* 3 4) has an even amount of lparens and rparens,
            // but they are not placed properly.
            if rparens == lparens { return Err(BAD_EXPR) }
            lparens += 1 }
        if c == ')' { 
            rparens += 1;
            // At no point in a properly formatted expression will there
            // be more rparens than lparens.
            if rparens > lparens {
                return Err(BAD_EXPR)
            }
        }
    }

    if lparens != rparens { return Err(BAD_EXPR) }

    let mut terms: Vec<~str> = Vec::new();
    let mut tokens;

    match tokenizer(expr) {
        Ok(good_tokens) => { tokens = good_tokens },
        Err(msg)        => { return Err(msg)    },
    }

    Ok(tokens)
}

/// Inspects a string, or a slice of a string, and returns a Result. On success,
/// it returns an owned array of a tuple holding a token's type, and its value.
/// If a word in the expression is not a valid token, returns an error message.
pub fn tokenizer(expr: &str) -> Result<~[(Token, ~str)], &str> {

    let mut tokens: Vec<(Token, ~str)> = Vec::new();
    let mut word_buffer = StrBuf::new();

    let inspect_string = |word_buffer: &str| -> (bool, Token) {
        for operator in OPERATORS.iter() {
            if word_buffer == *operator { return (true, Operator) }
        }
        //there's little point in breaking when each operator is unique and
        //there are so few of them

        if word_buffer == "true" || word_buffer == "false" {
            return (true, Boolean)
        }

        for c in word_buffer.chars() {
            match c {
                '0' .. '9'  => { }, //numeric literal
                _           => { return (false, Invalid) },
            }
        }

        (true, Numeric) //it's a numeric expression
    };

    for current_char in expr.chars() {
        match current_char {
            'a'..'z'|'A'..'Z'|'0'..'9' | '.' => {
                word_buffer.push_char(current_char)
            }

            '+' | '-' | '*' | '/' | '%' => word_buffer.push_char(current_char),

            ' '         => {
                let (valid_token, token_type) = inspect_string(word_buffer.to_str());
                if valid_token == false { return Err("Invalid token") }
                tokens.push((token_type, word_buffer.to_str().to_owned()));
            }

            '('         => {
                match word_buffer.len() {
                    0   => { tokens.push((Lparen, "(".to_owned() )) },
                    _   => { 
                        let (valid_token, token_type) = inspect_string(word_buffer.to_str());
                        if valid_token == false { return Err("Invalid token") }

                        tokens.push((Lparen, "(".to_owned() ))
                    }
                }
            }

            ')'         => {
                match word_buffer.len() {
                    0   => { tokens.push((Lparen, "(".to_owned() )) },
                    _   => {
                        let (valid_token, token_type) = inspect_string(word_buffer.to_str());
                        if valid_token == false { return Err("Invalid token") }

                        tokens.push((Rparen, ")".to_owned() ))
                    }
                }
            }

            _           => { return Err("Invalid token") }
        }
    }

    Ok(tokens.as_slice().to_owned())
}

fn main() { println!("foo"); }
