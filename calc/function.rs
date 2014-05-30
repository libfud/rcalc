//! Evaluate functions defined by the user

extern crate collections;

use self::collections::HashMap;
use super::{CalcResult, funfind, Environment, Evaluate};
use super::literal::{BigNum, Boolean, Matrix, Symbol};
use super::tokenize::{TokenStream, Literal, LParen, RParen, LBracket, RBracket, Operator, Variable};
use super::operator::unbox_it;
use super::operator;

///Returns a function's name if it's already defined, or an error if it is not found.
pub fn from_str(name: &str, env: &mut Environment) -> CalcResult<String> {
    match funfind(&name.to_str(), env) {
        Ok(_)   => Ok(name.to_str().clone()),
        Err(_)  => Err("Unknown function: ".to_str().append(name.to_str().as_slice()))
    }
}

///Returns the value of the function for the arguments given
pub fn eval(fn_name: &String, args: &Vec<Box<Evaluate>>, env: &mut Environment) -> CalcResult {
    let (args_to_fulfill, funcstr) = try!(funfind(fn_name, env));

    if args.len() != args_to_fulfill.len() {
        return Err("Improper list of arguments!".to_str())
    }

    //Reconvert Symbol(String) to String, append to vector for use in hashmap
    let mut args_strs = Vec::new();
    for arg in args_to_fulfill.iter() {
        match arg {
            &Symbol(ref x)  => args_strs.push(x.clone()),
            _   => fail!("Unexpected argument type!")
        }
    }

    //Unbox the substitutions.
    let subs_literals = try!(unbox_it(args, env));

    let mut subs_strings: Vec<String> = Vec::new();

    //Populate the vector of substition strings
    for literal in subs_literals.iter() {
        match literal {
            &BigNum(ref x)   => subs_strings.push(x.to_str()),
            &Boolean(ref x)  => subs_strings.push(x.to_str()),
            &Matrix(ref x)   => subs_strings.push(x.to_str()),
            _   => fail!("unbox it returned something it shouldn't have!")
        }
    }

    //Use hashmap to rewrite string
    let mut sub_map: HashMap<String, String> = HashMap::new();

    //Populate the hashmap with the arguments as keys as the
    //substition strings as values
    for (arg, sub) in args_strs.iter().zip(subs_strings.iter()) {
        sub_map.insert(arg.clone(), sub.clone());
    }

    let mut tokens = TokenStream {
        expr: funcstr,
        index: 0u
    };

    let mut evaluable_string: String = String::new();

    //This is horribly inefficient
    loop {
        let maybe_token = match tokens.next() {
            Some(x) => x,
            None    => break
        };

        let token = match maybe_token {
            Ok(x)   => x,
            Err(m)  => return Err(m)
        };

        let sub_str = match token {
            LParen      => "( ".to_str(),
            RParen      => ") ".to_str(),
            LBracket    => "[ ".to_str(),
            RBracket    => "] ".to_str(),
            Literal(x)  => match x {
                BigNum(y)   => y.to_str().append(" "),
                Boolean(y)  => y.to_str().append(" "),
                _   => fail!("Unexpected type!")
            },
            Operator(x) => operator::to_str(&x).append(" "),
            Variable(x) => { //here's the magic
                let x_to_str = match sub_map.find(&x) {
                    Some(val)   => val.clone(),
                    None        => {
                        return Err("Variable used in expression not dislcosed in args!".to_str())
                    }
                };
                x_to_str.append(" ")
            },
            _   => return Err("Unexpected type for function!".to_str())
        };
        
        evaluable_string = evaluable_string.append(sub_str.as_slice());
    }
    
    super::eval(evaluable_string.as_slice(), env)
}
