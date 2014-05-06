//! Functions and variables which are used in more than one module.

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
pub static ONE_ARG_ONLY : &'static str =
    "This function only takes one argument!";

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
