//! Functions and variables which are used in more than one module.

pub static DESPAIR: &'static str = "Laundry day is a very dangerous day.";
pub static ONE_ARG_ONLY : &'static str =
    "This function only takes one argument!";
pub static TWO_ARGS_ERR : &'static str = "This function only takes two terms!";

pub static E: f64 = 2.71828182845904523536028747135266250_f64;

/// Function to convert an array of owned strings into f64 for work.
pub fn str_to_f64(str_array: &[~str]) -> Result<~[f64], &str> {
    let mut float_vec = Vec::new();
    for elem in str_array.iter() {
        match from_str::<f64>(*elem) {
            Some(num)   => { float_vec.push(num) },
            _           => { return Err(DESPAIR) }
        }
    }

    Ok(float_vec.as_slice().to_owned())
}

