//! Operators

use super::{Evaluate, CalcResult, Environment, lookup, funfind};
use super::common::{rational_to_f64_trig, str_to_rational};
use super::literal::{LiteralType, Boolean, Matrix, BigNum, Symbol, Func};

pub mod power;
pub mod arithmetic;

#[deriving(Show)]
#[deriving(Clone)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Sin,
    Cos,
    Tan,
    Rad,
    Deg,
    Eq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    If,
    Define,
    Defun,
    Print,
    Limit,
}

pub fn from_str(s: &str) -> Option<OperatorType> {
    match s {
        "+"     => Some(Add),
        "-"     => Some(Sub),
        "*"     => Some(Mul),
        "/"     => Some(Div),
        "pow"   => Some(Pow),
        "sin"   => Some(Sin),
        "cos"   => Some(Cos),
        "tan"   => Some(Tan),
        "rad"   => Some(Rad),
        "deg"   => Some(Deg),
        "<"     => Some(Lt),
        "<="    => Some(LtEq),
        "="     => Some(Eq),
        ">="    => Some(GtEq),
        ">"     => Some(Gt),
        "if"    => Some(If),
        "define"=> Some(Define),
        "defun" => Some(Defun),
        "print" => Some(Print),
        "lim"   => Some(Limit),
        _       => None
    }
}

pub fn to_str(op: &OperatorType) -> String {
    let answer = match *op {
        Add     => "+",
        Sub     => "-",
        Mul     => "*",
        Div     => "/",
        Pow     => "pow",
        Sin     => "sin",
        Cos     => "cos",
        Tan     => "tan",
        Rad     => "rad",
        Deg     => "deg",
        Lt      => "<",
        LtEq    => "<=",
        Eq      => "=",
        GtEq    => ">=",
        Gt      => ">",
        If      => "if",
        Define  => "define",
        Defun   => "defun",
        Print   => "print",
        Limit   => "lim"
    };

    answer.to_str()
}

pub fn unbox_it(args:&Vec<Box<Evaluate>>, env: &mut Environment) 
                                                -> Result<Vec<LiteralType>, String> {
    let mut literal_vec: Vec<LiteralType> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        let val = try!(args.get(i).eval(env));
        literal_vec.push( match val {
            Symbol(ref var) => try!(lookup(var, env)),
            Func(ref var)   => {
                let (_, fun) = try!(super::funfind(var, env));
                Func(fun)
            },
            _   => val
        });
        i += 1;
    }

    Ok(literal_vec)
}

pub fn big_bool_matrix(args: &Vec<LiteralType>) -> (bool, bool, bool) {
    let mut bignum_flag = false;
    let mut bool_flag = false; //lol
    let mut matrix_flag = false;

    for literal in args.iter() {
        match literal {
            &BigNum(_)  => bignum_flag = true,
            &Boolean(_) => bool_flag = true,
            &Matrix(_)  => matrix_flag = true,
            &Symbol(_)  => { } //do nothing
            &Func(_)    => { } //do nothing
        }
    }

    (bignum_flag, bool_flag, matrix_flag)
}

pub fn find_matrix_len(args: &Vec<LiteralType>) -> Result<uint, String> {
    let mut matrix_len = 0;

    //get length of first matrix
    for literal in args.iter() {
        match literal {
            &Matrix(ref x)   => {
                matrix_len = x.len();
                break;
            }
            _           => { } //do nothing
        }
    }

    //see if all matrices are of a uniform length
    for literal in args.iter() {
        match literal {
            &Matrix(ref x)   => {
                if x.len() != matrix_len {
                    return Err("Mismatched matrices.".to_str())
                }
            }
            _           => { } //do nothing
        }
    }
    
    if matrix_len == 0 {
        Err("0 length matrices are not allowed!".to_str())
    } else {
        Ok(matrix_len)
    }
}

pub fn eval(op_type: OperatorType, args: &Vec<Box<Evaluate>>, env: &mut Environment)
                                                                            -> CalcResult {
    match op_type {
        Define  => {
            if args.len() != 2 {
                return Err("Define doesn't work that way!".to_str())
            }

            let var = match try!(args.get(0).eval(env)) {
                Symbol(ref x)   => x.clone(),
                _               => {
                    return Err("Attempted illegal definition!".to_str())
                }
            };

            let val = match try!(args.get(1).eval(env)) {
                Symbol(ref x)   => try!(lookup(x, env)),
                _   => try!(args.get(1).eval(env).clone())
            };
            env.vars.insert(var.clone(), val);
            Ok(Symbol(var))
        },

        Defun => {
            if args.len() != 1 {
                return Err("bad use of defun!".to_str())
            }
            let fn_string = match try!(args.get(0).eval(env)) {
                Func(ref x) => x.clone(),
                _           => {
                    return Err("Attempted illegal defunition!".to_str())
                }
            };

            let fn_string = fn_string.as_slice().trim();

            if fn_string.len() == 0 {
                fail!("Impossible fn length!")
            }
            let symbol = fn_string.words().next().unwrap();
            match symbol.chars().next().unwrap() {
                'a'..'z'|'A'..'Z' => { }, //okay
                _   => {
                    return Err("Illegal function name!".to_str())
                }
            }

            if fn_string.len() == symbol.len() {
                return Err("Illegal function!".to_str())
            }

            let fn_string = fn_string.slice_from(symbol.len()).trim();
            if fn_string.starts_with("(") == false {
                return Err("No arguments found!".to_str())
            }

            let args_string_len = match fn_string.find(|c: char| c == ')') {
                Some(x) => x,
                None    => {
                    return Err("Illegal function!".to_str())
                }
            };

            let args_string = fn_string.slice(1, args_string_len + 1);
            let mut arguments: Vec<LiteralType> = Vec::new();
            for arg in args_string.slice_to(args_string_len).words() {
                if arg.ends_with(")") {
                    arguments.push(Symbol(arg.slice_to(arg.len() - 1).to_str()))
                } else {
                    arguments.push(Symbol(arg.to_str()))
                }
            }

            if fn_string.len() == args_string.len()  {
                return Err("No procedure found!".to_str())
            }

            let fn_string = fn_string.slice_from(args_string.len() + 1).trim();

            if fn_string.starts_with("(") && fn_string.ends_with(")") {
                env.funs.insert(symbol.to_str(), (arguments, fn_string.to_str()));
            } else {
                println!("{}", fn_string);
                return Err("Illegal fn!".to_str())
            }

            Ok(Func(symbol.to_str()))
        },

        Print   => {
            let literal_vec = try!(unbox_it(args, env));
            for term in literal_vec.iter() {
                match *term {
                    BigNum(ref x)   => println!("{}", x),
                    Boolean(ref x)  => println!("{}", x),
                    Matrix(ref x)   => println!("{}", x),
                    Symbol(ref x)   => {
                        let val = match lookup(x, env) {
                            Ok(value)  => value.to_str(),
                            Err(_)     => match super::funfind(x, env) {
                                Ok((args, x))   => (args, x).to_str(),
                                Err(msg)    => msg
                            }
                        };
                        println!("{}", val)
                    },
                    Func(ref x)     => println!("{}", x),
                }
            }

            Ok(Boolean(true)) //this is a poor workaround
        },

        Limit   => {
            Ok(Symbol("foo".to_str()))
        },

        Add => arithmetic::add(args, env),

        Sub => arithmetic::sub(args, env),

        Mul => arithmetic::mul(args, env),

        Div => arithmetic::div(args, env),

        Pow => power::pow_wrapper(args, env),

        If  => {
            if args.len() != 3 {
                return Err("'if' requires three arguments".to_str())
            } 
            
            let condition = match try!(args.get(0).eval(env)) {
                Boolean(x)  => x,
                Symbol(x)   => {
                    match try!(lookup(&x, env)) {
                        Boolean(y)  => y,
                        _   => return Err("Only booleans can be a condition!".to_str())
                    }
                },
                _           => { return Err("Only booleans can be a condition!".to_str()) }
            };
                
            if condition == true {
                Ok(try!(args.get(1).eval(env)))
            } else {
                Ok(try!(args.get(2).eval(env)))
            }
        },

        Sin => {
            if args.len() > 1 {
                return Err("'sin' takes one argument".to_str())
            }

            let evaluated_array = try!(unbox_it(args, env));
            let evaluated = match evaluated_array.as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => {
                    return Err("I'm too tired to do this right now.".to_str())
                },
            };
            
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.sin().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_str()) }
            };
            
            Ok(BigNum(answer))
        },

        Cos => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_str())
            }
            let evaluated = match (try!(unbox_it(args, env))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Something went wrong".to_str()) }
            };
                
            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = ration_as_float.cos().to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_str()) }
            };
            
            Ok(BigNum(answer))
        },

        Tan => {
            if args.len() > 1 {
                return Err("'cos' takes one argument".to_str())
            }
            let evaluated = match (try!(unbox_it(args, env))).as_slice()[0] {
                BigNum(ref x)   => x.clone(),
                _           => { return Err("Too tired".to_str()) }
            };

            let ration_as_float = rational_to_f64_trig(&evaluated);

            let penult_answer = (ration_as_float.sin() / ration_as_float.cos()).to_str();
            let answer = match str_to_rational(&[penult_answer]) {
                Ok(array)   => array[0],
                Err(msg)    => { return Err(msg.to_str()) }
            };
            
            Ok(BigNum(answer))
        },

        Rad => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_str())
            }
            
            let degrees = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let radians = degrees.mul(&pi.div(&one80));

            Ok(radians) */
            Ok(Boolean(true))
        },

        Deg => { /*
            if args.len() != 1 {
                return Err("'rad' takes one argument".to_str())
            }

            let radians = try!(args.get(0).eval());
            let pi: BigRational = big_pi();
            let one80: BigRational = half_circ();

            let degrees = radians.mul(&one80.div(&pi));

            Ok(degrees)
            */
            Ok(Boolean(true))
        },

        Lt  => {
            if args.len() != 2 {
                return Err("< requires two arguments".to_str())
            }

            let (arg1, arg2) = (try!(args.get(0).eval(env)), try!(args.get(1).eval(env)));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 < arg2)),
                _   => Err("Nonboolean".to_str())
            }
        },

        LtEq => {
            if args.len() != 2 {
                return Err("<= requires two arguments".to_str())
            }
            let (arg1, arg2) = (try!(args.get(0).eval(env)), try!(args.get(1).eval(env)));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 <= arg2)),
                _   => Err("Non boolean".to_str())
            }
        },

        Eq  => {
            if args.len() != 2 {
                return Err("= requires two arguments".to_str())
            }

            let arg1 = try!(args.get(0).eval(env));
            let arg2 = try!(args.get(1).eval(env));
            match (arg1, arg2) {
                (BigNum(x), BigNum(y))  => Ok(Boolean(x == y)),
                _                       => Err("oh snap".to_str())
            }
        },

        GtEq => {
            if args.len() != 2 {
                return Err(">= requires two arguments".to_str())
            }

            let (arg1, arg2) = (try!(args.get(0).eval(env)), try!(args.get(1).eval(env)));
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 >= arg2)),
                _                       => Err("something".to_str())
            }
        },
        
        Gt   => {
             if args.len() != 2 {
                return Err(">= requires two arguments".to_str())
            }

            let comparands = try!(unbox_it(args, env));
            let (arg1, arg2) = (comparands.get(0).clone(), comparands.get(1).clone());
            match (arg1.clone(), arg2.clone()) {
                (BigNum(_), BigNum(_))  => Ok(Boolean(arg1 > arg2)),
                _                       => Err("blug".to_str())
            }
        }
    }
}
