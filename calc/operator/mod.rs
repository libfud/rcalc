//operators

extern crate num;

use self::num::rational::BigRational;
use std::num;
use super::{Evaluate, CalcResult, Environment, lookup};
use super::common::{rational_to_f64_trig, str_to_rational};
use super::literal::{LiteralType, Boolean, Matrix, BigNum, Symbol, Func};

pub mod power;

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

            Ok(Func(fn_string.to_str()))
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

            Ok(Symbol("foo".to_str()))
        },

        Limit   => {
            Ok(Symbol("foo".to_str()))
        },

        Add => {
            let literal_vec = try!(unbox_it(args, env));

            fn add_big(terms: &Vec<BigRational>) -> BigRational {
                let mut sum: BigRational = num::zero();
                for x in terms.iter() { sum = sum.add(x) }
                sum
            };

            let matrix_add = | terms: &Vec<Vec<BigRational>> | -> Vec<BigRational> {
                let mut sum_vec: Vec<BigRational> = Vec::new();
                let matrix_len= match find_matrix_len(&literal_vec) {
                    Ok(val) => val,
                    Err(_) => 0, //this shouldn't happen
                };
                let len = terms.len();

                for i in range(0u, matrix_len) {
                    let mut column: Vec<BigRational> = Vec::new();
                    for x in range(0u, len) {
                        column.push(terms.as_slice()[x].as_slice()[i].clone());
                    }
                    sum_vec.push(add_big(&column.clone()));
                }

                sum_vec
            };

            let (bignum_flag, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            match (bignum_flag, matrix_flag, bool_flag) {
                (false, false, false)   => { Ok(BigNum(num::zero())) }
                (_, _, true)    => {
                    Err("Attempted addition with boolean value!".to_str())
                },

                (true, true, false) => {
                    Err("Cannot currently add matrices and bignums".to_str())
                },
                (true, false, false)    => {
                    let mut vec_bigs: Vec<BigRational> = Vec::new();
                    for term in literal_vec.iter() {
                        vec_bigs.push(match *term{
                            BigNum(ref x)   => x.clone(),
                            Symbol(ref x)   => {
                                let val = try!(lookup(x, env));
                                match val {
                                    BigNum(ref y) => y.clone(),
                                    _   => return Err("impossible!".to_str())
                                }
                            }
                            _           => return Err("impossible".to_str())
                        });
                    }
                    Ok(BigNum(add_big(&vec_bigs.clone())))
                },
                (false, true, false)    => {
                    let mut vec_matrix: Vec<Vec<BigRational>> = Vec::new();
                    for term in literal_vec.iter() {
                        vec_matrix.push(match *term {
                            Matrix(ref x)   => x.clone(),
                            _           => return Err("impossible".to_str())
                        });
                    }
                    Ok(Matrix(matrix_add(&vec_matrix.clone())))
                }
            }
        },

        Sub => {
            if args.len() < 1 {
                return Err("Subtraction requires at least one argument".to_str())
            }

            let literal_vec = try!(unbox_it(args, env));

            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted subtraction with boolean value!".to_str())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            let zero: BigRational = num::zero();

            fn bignum_sub(head: BigRational, tail: &[LiteralType]) -> BigRational {
                let mut difference = head.clone();

                for term in tail.iter() {
                    match *term {
                        BigNum(ref x) => difference = difference.sub(x),
                        _             => { },
                    }
                }

                difference
            }

            fn matrix_sub(matrix_len: uint, head: &[BigRational], tail: &[LiteralType],
                    env: &mut Environment) -> Result<Vec<BigRational>, String> {

                let mut diff_vec: Vec<BigRational> = Vec::new();
                for i in range(0u, matrix_len) { diff_vec.push(head[i].clone()); }

                for literal in tail.iter(){
                    match *literal {
                        BigNum(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let diff = diff_vec.as_slice()[i].sub(x);
                                diff_vec.as_mut_slice()[i] = diff;
                            }
                        },
                        Boolean(_)  => { }, //no booleans
                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let diff = diff_vec.as_slice()[i].sub(&x.as_slice()[i]);
                                diff_vec.as_mut_slice()[i] = diff;
                            }
                        },
                        Symbol(ref x)   => {
                            let val = try!(lookup(x, env));
                            match val {
                                BigNum(ref y)   => {
                                    for i in range(0u, matrix_len) {
                                        let diff = diff_vec.as_slice()[i].sub(y);
                                        diff_vec.as_mut_slice()[i] = diff;
                                    }
                                },
                                Boolean(_)      => {
                                    return Err("Boolean detected in sub operation!".to_str())
                                }
                                Matrix(ref y)   => {
                                    if y.len() != matrix_len {
                                        return Err("Mismatched matrices!".to_str())
                                    }
                                    for i in range(0u, matrix_len) {
                                        let diff = diff_vec.as_slice()[i].sub(&y.as_slice()[i]);
                                        diff_vec.as_mut_slice()[i] = diff;
                                    }
                                },
                                Symbol(_)       => {
                                    return Err("2deep4me".to_str())
                                },
                                Func(_)         => {
                                    return Err("2deep4me".to_str())
                                },
                            }
                        },
                        Func(_) => { } //idk yet
                    }
                }

                Ok(diff_vec)
            }

            if matrix_flag == false {
                if args.len() == 1 {
                    Ok(BigNum(bignum_sub(zero, literal_vec.as_slice())))
                }
                else {
                    let head = match literal_vec.as_slice()[0] {
                        BigNum(ref x)   => x.clone(),
                        _               => zero.clone()
                    };

                    Ok(BigNum(bignum_sub(head, literal_vec.slice_from(1))))
                }
            } else { 
                let mut head: Vec<BigRational> = Vec::new();
                let mut head_i = 0;

                if args.len() == 1 { 
                    for _ in range(0u, matrix_len) { head.push(zero.clone()); }
                } else {
                    head_i = 1;
                    match literal_vec.as_slice()[0] {
                        BigNum(_)       => {
                            return Err(
                            "Illegal subtraction operation! Cannot subtract matrix from bignum"
                                        .to_str())
                        },

                        Boolean(_)      => { }, //there are no booleans

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                head.push(x.as_slice()[i].clone());
                            }
                        }
                        Symbol(ref x)   => {
                            let val = try!(lookup(x, env));
                            match val {
                                BigNum(_)   => {
                                    return Err("Cannot sub matrix from bignum!".to_str())
                                },
                                Boolean(_)  => {
                                    return Err("Attempted to use boolean for sub!".to_str())
                                },
                                Matrix(ref y)   => {
                                    if y.len() != matrix_len {
                                        return Err("Mismatched matrices!".to_str())
                                    }
                                    for i in range(0u, matrix_len) {
                                        head.push(y.as_slice()[i].clone());
                                    }
                                },
                                Symbol(_)   => {
                                    return Err("2deep4me".to_str())
                                },

                                Func(_)     => {
                                    return Err("WAY 2 deep 5 me".to_str())
                                },
                            }
                        },
                        Func(_) => { } //idk yet
                    }
                }

                let answer = try!(matrix_sub(matrix_len, head.as_slice(),
                                                    literal_vec.slice_from(head_i), env));
                Ok(Matrix(answer))
            }
        },

        Mul => {
            let literal_vec = try!(unbox_it(args, env));

            let (_,bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted multiplication with boolean value!".to_str())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            let one: BigRational = num::one();

            if matrix_flag == false {
                let mut product = one.clone();
                for literal in literal_vec.iter() {
                    match *literal {
                        BigNum(ref x)   => product = product.mul(x),
                        _               => { }//do nothing
                    }
                }

                Ok(BigNum(product))
            } else {
                let mut prod_vec: Vec<BigRational> = Vec::new();
                for _ in range(0u, matrix_len) { prod_vec.push(one.clone()) }

                for literal_x in literal_vec.iter(){
                    match *literal_x {
                        Boolean(_)      => { }, //do nothing

                        Func(_)         => { }, //idk yet

                        BigNum(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let product = prod_vec.as_slice()[i].mul(x);
                                prod_vec.as_mut_slice()[i] = product;
                            }
                        },

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                let product = prod_vec.as_slice()[i].mul(&x.as_slice()[i]);
                                prod_vec.as_mut_slice()[i] = product;
                            }
                        },
                        Symbol(ref x)   => {
                            let val = try!(lookup(x, env));
                            match val {
                                Boolean(_)  => {
                                    return Err("Attempted to use boolean in multiplication!".to_str())
                                }
                                BigNum(ref x) => {
                                    for i in range(0u, matrix_len) {
                                        let product = prod_vec.as_slice()[i].mul(x);
                                        prod_vec.as_mut_slice()[i] = product;
                                    }
                                },

                                Func(_)         => { }, //idk yet
                                Matrix(ref x)   => {
                                    if x.len() != matrix_len {
                                        return Err("Mismatched matrices!".to_str())
                                    }
                                    for i in range(0u, matrix_len) {
                                        let product = prod_vec.as_slice()[i].mul(&x.as_slice()[i]);
                                        prod_vec.as_mut_slice()[i] = product;
                                    }
                                },
                                Symbol(_)       => {
                                    return Err("2Deep4Me".to_str())
                                }
                            }
                        }
                    }
                }

                Ok(Matrix(prod_vec))
            }
        },

        Div => {
            if args.len() < 1 {
                return Err("Division requires at least one argument!".to_str())
            }

            let literal_vec = try!(unbox_it(args, env));

            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true {
                return Err("Attempted multiplication with Boolean value!".to_str())
            }

            let matrix_len;
            if matrix_flag == true {
                matrix_len = try!(find_matrix_len(&literal_vec));
            } else {
                matrix_len = 0;
            }

            fn bignum_div(head: BigRational, tail: &[LiteralType]) -> 
                Result<BigRational, String> {

                let mut quotient = head.clone();
                let zero: BigRational = num::zero();

                for term in tail.iter() {
                    match *term {
                        BigNum(ref x)   => {
                            if *x == zero { return Err("Division by zero!".to_str()) }
                            quotient = quotient.div(x);
                            }
                        _               => { },
                    }
                }

                Ok(quotient)
            }   

            fn matrix_div(matrix_len: uint, head: &[BigRational], tail: &[LiteralType]) ->
                Result<Vec<BigRational>, String> {
                
                let zero: BigRational = num::zero();
                let mut quot_vec: Vec<BigRational> = Vec::new();
                for i in range(0u, matrix_len) { quot_vec.push(head[i].clone()); }

                for literal in tail.iter() {
                    match *literal {
                        BigNum(ref x)   => {
                            if *x == zero {
                                return Err("Division by zero!".to_str())
                            }
                            for i in range(0u, matrix_len) {

                                let quotient = quot_vec.as_slice()[i].div(x);
                                quot_vec.as_mut_slice()[i] = quotient;
                            }
                        }
                        Boolean(_)      => { },
                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                if x.as_slice()[i] == zero {
                                    return Err("Division by zero!".to_str())
                                }
                                let quotient = quot_vec.as_slice()[i].div(&x.as_slice()[i]);
                                quot_vec.as_mut_slice()[i] = quotient;
                            }
                        },
                        Symbol(_)   => { } //push this off for now
                        Func(_)     => { } //push this off for now
                    }
                }

                Ok(quot_vec)
            }

            let one: BigRational = num::one();
            if matrix_flag == false {
                if args.len() == 1 {
                    let answer = try!(bignum_div(one, literal_vec.as_slice()));

                    Ok(BigNum(answer))
                } else {
                    let head = match literal_vec.as_slice()[0] {
                        BigNum(ref x)   => x.clone(),
                        _               => one.clone(), //shouldn't happen, see matrix_flag
                    };

                    let answer = try!(bignum_div(head, literal_vec.slice_from(1)));

                    Ok(BigNum(answer))
                }
            } else {
                let mut head: Vec<BigRational> = Vec::new();
                let mut head_i = 0;

                if args.len() == 1 {
                    for _ in range(0u, matrix_len) { head.push(one.clone()); }
                } else {
                    head_i = 1;

                    match literal_vec.as_slice()[0] {
                        BigNum(_)       => {
                            return Err("Illegal division operation! ".to_str().append( 
                            "Cannot divide bignum by matrix!"))
                        },

                        Boolean(_)      => {}, //booleans already caused failure if present

                        Func(_)         => {
                            return Err("idk yet".to_str())
                        }

                        Matrix(ref x)   => {
                            for i in range(0u, matrix_len) {
                                head.push(x.as_slice()[i].clone());
                            }
                        },

                        Symbol(ref x)   => {
                            match try!(lookup(x, env)) {
                                Func(_)     => {
                                    return Err("Idk yet".to_str())
                                },
                                BigNum(_)   => {
                                    return Err("Can't divide bignum by matrix!".to_str())
                                },
                                Boolean(_)  => {
                                    return Err("Attempted division with boolean!".to_str())
                                },
                                Matrix(ref x)   => {
                                    if x.len() != matrix_len {
                                        return Err("Mismatched matrices!".to_str())
                                    }
                                    for i in range(0u, matrix_len) {
                                        head.push(x.as_slice()[i].clone());
                                    }
                                },
                                Symbol(_)   => {
                                    return Err("2deep4me".to_str())
                                }
                            }
                        }
                    }
                }

                let answer = try!(matrix_div(matrix_len, head.as_slice(),
                    literal_vec.slice_from(head_i)));

                Ok(Matrix(answer))
            }
        },

        Pow => {
            let literal_vec = try!(unbox_it(args, env));
            let (_, bool_flag, matrix_flag) = big_bool_matrix(&literal_vec);
            if bool_flag == true || matrix_flag == true {
                return Err("Not yet...".to_str())
            }
            power::pow_wrapper(args, env) },

        If  => {
            if args.len() != 3 {
                return Err("'if' requires three arguments".to_str())
            } 
            
            let condition = match try!(args.get(0).eval(env)) {
                Boolean(x)  => x,
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

