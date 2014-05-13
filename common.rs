//! Functions and variables which are used in more than one module.

extern crate num;

use self::num::rational::{BigRational, Ratio};
use std::io::{BufferedReader, BufferedWriter, File, Open };
use std::io::{Read, Write};
use std::num;
use search::binary_search;

pub mod search;

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
pub static ONE_ARG_ONLY : &'static str =
    "This function only takes one argument!";
pub static TWO_ARGS_ERR : &'static str = "This function only takes two terms!";

pub static E: f64 = 2.71828182845904523536028747135266250_f64;

pub fn help(list: &str) {
    let help_help =
"The help function has the form (help term1, term2, term3...) and prints out
examples of how operators are used. You can use help for individual operators
for individual operators, and you can list operators by group with the 
following terms: arithmetic, logic, trigonometry (or trig), and statistics
(or stats). See also (help use). An example of usage is

(+ 2 2)";

    let use_help =
"rcalc is an arbitrary precision polish notation (lisp style) calculator.
It requires that expressions are wrapped in parentheses ( \"( and \") ).
The operator goes first in any expression, followed by its terms.
For an  example, the following returns -3/2:

(/ (- 6 (pow 9 (/ 2)) (+ -4.5 13/2)))

As you can see, you can nest expressions. Not all expressions are evaluated:

(+ 2 (if (> 3 2) (+ 7 3) (* 2)))

only (> 3 2) and (+ 7 3) are evaluated in that case, and then added to 2.

There are a variety of acceptable input formats for numebers. You can prepend
a negative symbol to the numerator or denominator, use fractions, or numbers
that have a explicit radix (decimal) point. However, mixing explicit radix
points with fractional notation is disallowed, and the preferred method
is either just an integer as a numerator or an explicit fraction. If you
want to express the reciprocal of a number, either input it as 1/n, where n is
a numeric literal, or as (/ n). Input as numbers with an explicit radix 
is quite buggy due to the limitations of IEEE754.";

    let abs_help =
"The absolute function. Takes only one term, and returns its absolute value.

(abs -5.5) -> 11/2
(abs 17/7) -> 17/7";
    
    let arithmetic_help =
"The arithmetic operators are +, -, *, /, %, and pow.
In general, the arithmetic operators can take 0 terms, or as many terms as you
like. For example, pow acts like a tower of power.

(pow 2 3 4)

is equivalent to (pow 2 81).
To see more help, use help with the appropriate arithmetic operator.
Aadditionally, see (help abs), as I have not classified it yet.";

    let stats_help =
"Statistical functions. Currently only comprised of avg, which returns the
arithmetic mean of a list of terms.

(avg 42) -> 42/1
(avg 7.5 12 -13.25 4/5) -> 161/80";

    let add_help =
"The addition operator. If no terms are supplied, returns the the additive
identity, 0. The examples below are valid expressions:

(+ ) -> 0/1
(+ 7) -> 7/1
(+ 2 2) -> 4/1
(+ 3 4 (/ 25 5) 6) -> 18/1
(+ -3.5) -> -7/2 ";

    let sub_help = 
"The subtraction operator. Requires at least one term. If only one term is
given, it returns the negation of that term. The following are examples of
valid expressions:

(- 2) -> -2/1
(- -1.5) -> 3/2
(- 3 2) -> 1/1
(- 10 3 3) -> 4/1
(- (+ 2 2) 1) -> 3/1";

    let mul_help =
"The multiplication operator. If zero terms are given, returns the
multiplicative identity, one. The following are examples of valid
expressions:

(* ) -> 1/1
(* -2/3 -3/2) -> 1/1
(* 2 3 4) -> 24/1
(* .5 .25) -> 1/8
(* 0.75 100) -> 750";

    let div_help =
"The division operator. Requires at least one term. If only one term is
given, Division by zero is undefined. it returns that term's reciprocal.
Valid examples of expressions:

(/ 2) -> 1/2
(/ 4/5 -1.25) -> -16/25
(/ 24 6 2) -> 2
(/ (pow 2 2 2) 4) -> 4
(/ 144 12 4 3) -> 1";

    let pow_help =
"The exponentiation operator. If supplied no arguments, returns the
multiplcative identity, 1. If only one term is supplied, the implied power is
1. Exponentiation with zero as a base is allowed, but has some notable
behavior:

(pow 0) -> 0/1     | 0 to any non zero power is 0.
(pow 0 0) -> 1/1   | 0^0 is one.
(pow 0 0 0) -> 0/1 | Problem?

If you are asking what's going on with 0, it's because pow is evaluated
recursively from left to right: if more than two terms are supplied, it
essentially evaluates from the rightmost two terms, treating the very
last as the power and the second to last as the base.

(pow 0 0 0) -> (pow 0 (pow 0 0)) -> (pow 0 1) -> 0/1

That's all I have to say on that matter. Below are valid expressions:

(pow 2 2 2 2) -> 65536/1
(pow 2 3 4)   -> 24178851639229258349412352/1
(pow 2 .5)    -> 577/408
(pow 27 1/3)  -> 3/1
(pow 256 1/8) -> 2/1
(pow 2 -1)    -> 1/2
(pow 8 -2)    -> 1/64";

    let trig_help =
"Trigonometric functions. Currently only comprised of sin, cos, tan, rad and
deg. Each function only takes one term, or an expression which is evaluated to
a single term.";

    let sin_help =
"The sine function. Takes one term. If no terms are supplied, it evaluates
zero, which is still zero. Uses radians, not degrees. If you want to
express the angle in degrees, convert with rad.

(sin pi)    -> 0
(sin (* 1/2 pi)) -> 1
(sin (/ (* 2 pi) 3)) -> 0.866025
(sin (rad 90)) -> 1";

    let cos_help =
"The cosine function. Takes one term. If no terms are supplied, it evaluates
zero. Uss radians. If you want to express the angle in degrees, convert with
the rad function

(cod ) -> 1/1
(cos pi) -> -1
(cos (rad 270)) -> 0/1
(cos (rad 60)) -> 0.5";

    let tan_help =
"The tangent function. Takes exactly one term.

(tan 1) -> 1.55
(tan pi) -> 0
(tan 0) -> 0
(tan (rad 45)) -> 1";

    let avg_help =
"Returns the arithmetic mean of a set of numbers. Requires at least one term.

(avg 2 3 4) -> 3/1
(avg 12.25 -7/3 -14) -> -49/36
(avg 42 6 7 13 9 9 9) -> 95/7";

    let logic_help =
"The ordering operators are <, <=, =, >=, >. Additionally, you can compose
conditional statements with if.

(> 3 2) -> true
(= 7 3) -> false
(if (> 3 2) (+ 7 3) (/ 10 2)) -> 10/1";

    let condit_help =
"The if statement takes three expressions as arguments: a conditional
statement, a consequent, and an alternative.

(if (> 3 2) (+ 7 3) (/ 10 2)) -> 10/1
(+ 2 (if (= (pow 2 2 2) (abs -16)) (+ 1 1) (* pi 2)) 2) -> 6/1";

    let lt_help =
"The ordering operator, <, takes two terms, and returns either true or false.

(< 3 2) -> true
(< 3 pi) -> false";

    let lte_help =
"The ordering operator, <=, takes two terms, and returns either true or false.

(<= 7 6) -> true
(<= 6/3 3/2) -> false";

    let eq_help =
"The ordering operator, =, takes two terms, and returns either true or false.

(= 13 13) -> true
(= pi e) -> false";

    let gte_help =
"The ordering operator, >=, takes two terms, and returns either true or false.

(>= 7 6) -> true
(>= pi 13/2) -> false";

    let gt_help =
"The ordering operator, >, takes two terms, and returns either true or false.

(> 9 5/4) -> true
(> e pi) -> false";

    if list.len() < 2 { println!("{}", help_help) }

    for term in list.words() {
        println!("{}", match term {
            "help"              => help_help,
            "use"               => use_help,
            "abs"               => abs_help,
            "arithmetic"        => arithmetic_help,
            "+"|"add"           => add_help,
            "-"|"subtraction"   => sub_help,
            "*"|"multiply"      => mul_help,
            "/"|"division"      => div_help,
            "pow"|"power"       => pow_help,
            "sin"|"sine"        => sin_help,
            "cos"|"cosine"      => cos_help,
            "tan"|"tangent"     => tan_help,
            "trig"              => trig_help,
            "stats"             => stats_help,
            "avg"               => avg_help,
            "<"                 => lt_help,
            "<="                => lte_help,
            "="                 => eq_help,
            ">="                => gte_help,
            ">"                 => gt_help,
            "if"                => condit_help,
            "logic"             => logic_help,
            _                   => "More help is not available at this time."
            }
        );
    }
}



/// Function to convert an array of owned strings into BigRationals for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_rational(str_array: &[~str]) -> Result<~[BigRational], &str> {
    let mut big_vec: Vec<BigRational> = Vec::new();
    let mut rational: BigRational;

    for elem in str_array.iter() {
        let number_type = get_number_type(*elem);
        if number_type == "invalid representation" {
            return Err(number_type)
        }
        match number_type {
            "fraction"      => {
                match from_str::<BigRational>(*elem) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return Err(DESPAIR)
                    }
                }
            },

            "non-fraction"  => {
                let mut floated: f64;
                match from_str::<f64>(*elem) {
                    Some(num)   => { floated = num },
                    _           => {
                        return Err(DESPAIR)
                    }
                }
                match Ratio::from_float(floated) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return Err(DESPAIR)
                    }
                }
            },

            _               => {
                return Err(DESPAIR)
            }
        }
        big_vec.push(rational);
    }

    Ok(big_vec.as_slice().to_owned())
}

/// Determines if a number is a decimal representation or a fractional
/// representation. Mixing is disallowed. Returns a message and the
/// location of the division symbol or radix point.
pub fn get_number_type(num_str: &str) -> &str {
    let bad_sym = "invalid representation";
    let mut div_symbol = false;
    let mut radix_point_symbol = false;

    if num_str.slice_to(1) == "/" ||
        num_str.slice_to(num_str.len() -1) == "/" { return bad_sym  }

    for c in num_str.chars() {
        match c {
            '/' => {
                if div_symbol == true { return bad_sym }
                else { div_symbol = true; }
            },
            '.' => {
                if radix_point_symbol == true { return bad_sym }
                else { radix_point_symbol = true; }
            }
            _   => { } // do nothing, it doesn't concern us
        }
    }

    if div_symbol == true && radix_point_symbol == true {
        bad_sym 
    } else if div_symbol == true {
        "fraction"
    } else {
        "non-fraction"
        // a number without either / or . has an implicit .
    }
}

/// Function to convert an array of owned strings into f64 for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_f64(str_array: &[~str]) -> (&str, ~[f64]) {
    let mut float_vec = Vec::new();
    for elem in str_array.iter() {
        match from_str::<f64>(*elem) {
            Some(num)   => { float_vec.push(num) },
            _           => { return (DESPAIR, [0.0].to_owned()) }
        }
    }

    ("OK!", float_vec.as_slice().to_owned())
}

/// Function to convert an array of owned strings into f32 for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_f32(str_array: &[~str]) -> (&str, ~[f32]) {
    let mut float_vec = Vec::new();
    for elem in str_array.iter() {
        match from_str::<f32>(*elem) {
            Some(num)   => { float_vec.push(num) },
            _           => { return (DESPAIR, [0f32].to_owned()) }
        }
    }

    ("OK!", float_vec.as_slice().to_owned())
}

/// Reads an array from file into a vector of strings
pub fn read_bignums_from_file(strpath: &str) ->
    Result<Vec<BigRational>, &str> {

    let path = Path::new(strpath);
    if path.exists() == false { return Err("Path does not exist") }

    let mut vec_bigrationals: Vec<BigRational> = Vec::new();

    let file = match File::open_mode(&path, Open, Read) {
        Ok(f) => f,
        Err(e) => fail!("file error: {}", e),
    };

    let mut reader = BufferedReader::new(file);

    for aline in reader.lines() {
        match from_str::<BigRational>(aline.ok().unwrap()) {
            Some(bignum)    => { vec_bigrationals.push(bignum) },
            _               => { return Err("Failed from str to rational!") }
        }
    };

    return Ok(vec_bigrationals)
}

/// write_bignums_to_file Takes an array of bignumbes, and casts them to
/// strings. It then writes these strings to a file. Because it uses
/// append mode, slices of new numbers added to the vector should be used;
/// ie, if you had 3 primes in the file to begin with, and you added two
/// more, give it a slice of the last two elements.
pub fn write_bignums_to_file(bignum_vec: &[BigRational], strpath: &str) {

    let path = Path::new(strpath);

    let file = File::open_mode(&path, Open, Write);
    let mut writer = BufferedWriter::new(file);

    for bignum in bignum_vec.iter() {
        let bignum_str = bignum.to_str();
        match writer.write_line(bignum_str) {
            Ok(f)   => f,
            Err(e)  => fail!("file error: {}", e)
        }
        match writer.flush() {
            Ok(f)   => f,
            Err(e)  => fail!("file error: {}", e)
        }
    }

}

/// Generates a list of prime numbers. If given an empty vector, starts from
/// 2. If given a vector with more than one element, it starts from the last
/// element in that vector. Uses two loops - the outer loop sets an index 
/// counter to zero, and a limit to 1/2 of the number (since using the square
/// root is sort of impossible considering that this function is a prereq to
/// enable that). The inner loop then checks if the index is greater than the
/// length of the vector or if the element at that index is greater than the
/// limit. If either of those conditions is true, that number is a prime, and
/// is pushed onto the vector. If the remainder of the number being tested by
/// the number at the index is zero, then the number is not prime, and the
/// guess is incremented by two.
pub fn prime_gen(mut prime_vec: Vec<BigRational>, end_at: BigRational) ->
    Vec<BigRational> {

    let zero :BigRational = num::zero();
    let one: BigRational = num::one();
    let two = one.add(&one);

    if prime_vec.len() < 1 {
        prime_vec.push(two.clone())
    }

    let mut pos_prime = match prime_vec.len() {
        1   => { prime_vec.as_slice()[0].add(&one) } // it is now three
        _   => { prime_vec.as_slice()[prime_vec.len() - 1].add(&two) }
        /* Okay, this bears some explanation. This is coming from a 
         * file that the user should not edit. This is a really terrible
         * assumption, but it would be insane to check that every number
         * after the first element is odd, considering there could be
         * millions. This is something that I will be taking on good
         * faith of the user, who, as we all know, is a good user who
         * doesn't just modify files that are recognizably a list of
         * prime numbers */
    };

    while pos_prime <= end_at.mul(&two) {
        let mut index: uint = 0;
        let lim = pos_prime.div(&two);
        loop {
            if index >= prime_vec.len() || prime_vec.as_slice()[index] > lim {
                //pos_prime is indivisible by any element in the array
                prime_vec.push(pos_prime.clone());
                break;
            }
            if pos_prime.rem(&prime_vec.as_slice()[index]) == zero { break }
            index += 1;
        }
        pos_prime = pos_prime.add(&two);
    }

    prime_vec
}

/// Function to determine if a number is prime. Compares it to a list
/// of known primes. If no list is available, a new one is generated.
/// if the last prime in the list of known primes is less than the number,
/// more primes are generated and added to the known primes.
pub fn is_prime(arbitrary_num: BigRational) -> bool {
    let mut primelist: Vec<BigRational> = Vec::new();
    let one: BigRational = num::one();

    if arbitrary_num == one { return false } // 1 is not prime.
    let search_begin: uint = 0;

    match read_bignums_from_file("primes.txt") {
        Ok(list_o_primes)   => {
            primelist = list_o_primes;
        },
        Err(_)              => {
            primelist = prime_gen(primelist, arbitrary_num.clone());
            write_bignums_to_file(primelist.as_slice(), "primes.txt");
        }
    }

    assert!(primelist.len() > 1);
    if arbitrary_num == primelist.as_slice()[primelist.len() - 1] {
        return false // it's prime
    } else if arbitrary_num > primelist.as_slice()[primelist.len() - 1] {
        primelist = prime_gen(primelist, arbitrary_num.clone());
        write_bignums_to_file(primelist.as_slice(), "primes.txt");
    }

    let search_end = primelist.len() - 1;
    assert!(search_begin < search_end);

    let (found, _) = binary_search(primelist, arbitrary_num, search_begin,
        search_end);

    found
}
