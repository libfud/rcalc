//! Tokenizes strings.
/* Largely adapted from Aochagavia's work, with modifications in mind for
 * BigRationals and some other items. Thank you very much, Aochagavia! */

extern crate num;

use self::num::rational::BigRational;

///Enumeration of valid tokens. Valid tokens are Operators, Literals, LParens,
///RParens, and Names.
#[deriving(Show)]
pub enum Token {
    Literal(BigRational),
    Boolean(bool),
    LParen,
    RParen,
    Operator(OperatorType),
    Name(StrBuf)
}

/// Tokenizs a string into 
pub fn tokenize(expr: &str) -> CalcResult<Vec<Token>> {
    let mut tokens = Vec::new();

    let mut i = 0;
    let len = expr.len();

    while i < len {
        let slice = expr.slice_from(i);

        //skip whitespace. unwrap is ok because of while i < len
        if slice.chars().next().unwrap().is_whitespace() {
            i += 1;
            continue;
        }

        //Parentheses
        let token = match slice.chars().next().unwrap() {
            '(' => Some(LParen),
            ')' => Some(RParen),
        };
        if token.is_some() {
            tokens.push(token.unwrap());
            i += 1;
            continue;
        }

        //Operators
        
        //There is at least one word, so it is safe to unwrap
        let word = slice.words().next().unwrap();

        //Discard dangling parens
        let word = word.slice(0, word.find(|c: char| c == ')' || c == '(').unwrap_or(word.len()));
        
        let token = match word {
            "+"     => Some(Operator(Add)),
            "-"     => Some(Operator(Sub)),
            "*"     => Some(Operator(Mul)),
            "/"     => Some(Operator(Div)),
            "%"     => Some(Operator(Rem)),
            "pow"   => Some(Operator(Pow)),
            "sin"   => Some(Operator(Sin)),
            "cos"   => Some(Operator(Cos)),
            "tan"   => Some(Operator(Tan)),
            "deg"   => Some(Operator(Deg)),
            "rad"   => Some(Operator(Rad)),
            "avg"   => Some(Operator(Avg)),
            "abs"   => Some(Operator(Abs)),
            "<"     => Some(Operator(Lt)),
            "<="    => Some(Operator(Lte)),
            "="     => Some(Operator(Equ)),
            ">="    => Some(Operator(Gte)),
            ">"     => Some(Operator(Gt)),
            "if"    => Some(Operator(Cond)),
            _       => None
        };
        if token.is_some() {
            tokens.push(token.unwrap());
            i += word.len(); //not all operators have a len of 1 ;)
            continue;
        }

        //Booleans
       
        let token = match word {
            "true"  => Some(Boolean(true)),
            "false" => Some(Boolean(false)),
            _       => None
        };
        if token.is_some() {
            tokens.push(token.unwrap());
            i += word.len();
            continue;
        }

        //Constants
       
        //token may be a numeric constant like pi or e or c
        let token = match word {
            "π" | "pi"  => Some(PI),
            "e"         => Some(E),
            _           => None
        }
        if token.is_some() {
            //constants should never fail a conversion, or something is really wrong
            let literal_token = from_str::<BigRational>(token).unwrap();
            tokens.push(Literal(literal_token));
            i += word.len();
            continue;
        }
        
        //Literals

        //no number should ever start or end with /
        if word.starts_with("/") || word.ends_with("/") {
            return Err(Owned(format!("Unrecognized token '{}'", word)))
        }

        let mut negative_sign_counter = 0;
        let mut radix_point_counter = 0;
        let mut fraction_counter = 0;

        for c in word.chars() {
            match c {
                '0'..'9'    => { }, //do nothing here
                '-'         => { negative_sign_counter += 1 },
                '.'         => { radix_point_counter += 1 },
                '/'         => { fraction_counter += 1 },
                _           => { return Err(Owned(format!("Unrecognized token '{}'", word))) }
            }
        }

        // Numbers could have a negative sign and, or (exclusively) a divisor or
        // a radix point.
        let token = match (fraction_counter, radix_point_counter, negative_sign_counter) {
            (0, 0, 0) | (1, 0, 0) | (0, 1, 0) => Some(word),

            (0, 0, 1) | (0, 1, 1) | (1, 0, 1) => {
                if word.starts_with("-") == true { Some(word) },
                else { return Err(Owned(format!("Unrecognized token '{}'", word))) }
            },

            _   => = None
        };
        if token.is_some() {
            match str_to_rational(token) {
                Ok(literal_val)    => {
                    tokens.push(literal_val);
                    i += word.len();
                    continue;
                }
                Err(msg)        => {
                    return Err(Owned(format!("Unrecognized token '{}'", word)))
                }
            }
        }
        
        //This point is reached if every other kind of token has not been matched
        return Err(Owned(format!("Unrecognized token '{}'", word)));
    }

    Ok(tokens)
}

