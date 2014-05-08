//! Functions and variables which are used in more than one module.

extern crate num;

use self::num::rational::{BigRational, Ratio};
use std::io::{BufferedReader, BufferedWriter, File, Open, Append};
use std::io::{Read, Write};
use std::num;
use search::binary_search;

pub mod search;

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
pub static ONE_ARG_ONLY : &'static str =
    "This function only takes one argument!";
pub static TWO_ARGS_ERR : &'static str = "This function only takes two terms!";

pub static E: f64 = 2.71828182845904523536028747135266250_f64;

/// Function to convert an array of owned strings into BigRationals for work.
/// A message is included to indicate the success of the operation.
pub fn str_to_rational(str_array: &[~str]) -> (&str, ~[BigRational]) {
    let mut big_vec: Vec<BigRational> = Vec::new();
    let mut rational: BigRational;

    for elem in str_array.iter() {
        let number_type = get_number_type(*elem);
        if number_type == "invalid representation" {
            return (number_type, big_vec.as_slice().to_owned())
        }
        match number_type {
            "fraction"      => {
                match from_str::<BigRational>(*elem) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
            },

            "non-fraction"  => {
                let mut floated: f64;
                match from_str::<f64>(*elem) {
                    Some(num)   => { floated = num },
                    _           => {
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
                match Ratio::from_float(floated) {
                    Some(bignum)    => { rational = bignum }
                    _               => { 
                        return (DESPAIR, big_vec.as_slice().to_owned())
                    }
                }
            },

            _               => {
                return (DESPAIR, big_vec.as_slice().to_owned())
            }
        }
        big_vec.push(rational);
    }

    ("OK!", big_vec.as_slice().to_owned())
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
pub fn write_bignums_to_file(bignum_vec: &[BigRational], strpath: &str)
    -> &str {

    let path = Path::new(strpath);
    if path.exists() == false { return "Path does not exist" }

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

    "success"
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

    while pos_prime <= end_at {
        let mut index: uint = 0;
        let lim = pos_prime.div(&two);
        loop {
            if index >= prime_vec.len() || prime_vec.as_slice()[index] > lim {
                //pos_prime is indivisible by any element in the array
                prime_vec.push(pos_prime.clone());
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
    let mut search_end: uint;

    match read_bignums_from_file("primes.txt") {
        Ok(list_o_primes)   => {
            primelist = list_o_primes;
            search_end = primelist.len() - 1;
        },
        Err(_)              => {
            primelist = prime_gen(primelist, arbitrary_num.clone());
            search_end = primelist.len() - 1;
            write_bignums_to_file(primelist.as_slice(), "primes.txt");
        }
    }

    assert!(primelist.len() > 1);
    if arbitrary_num == primelist.as_slice()[primelist.len() - 1] {
        return false // it's prime
    } else if arbitrary_num > primelist.as_slice()[primelist.len() - 1] {
        primelist = prime_gen(primelist, arbitrary_num.clone());
        write_bignums_to_file(primelist.slice_from(search_end), "primes.txt");
        search_end = primelist.len() - 1;
    }

    assert!(search_begin < search_end);

    let (found, _) = binary_search(primelist, arbitrary_num, search_begin,
        search_end);

    found
}
