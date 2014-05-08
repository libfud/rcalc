/// Binary search. Takes a generic aray whose type implements Ord, a key,
/// the index at which to begin, and the index at which to stop. Returns
/// a bool to indicate if it was found, and the index at which it was found.
pub fn binary_search<T: Ord>(list: Vec<T>, key: T, min_index_orig: uint,
    max_index_orig: uint) -> (bool, uint) {

    let mut found = false;
    let mut min_index = min_index_orig;
    let mut max_index = max_index_orig;
    let mut mid_index;

    if list.len() < 1 { return (found, 0 ) }

    loop {
        mid_index = (max_index + min_index) / 2;
        if list.as_slice()[mid_index] == key {
            found = true;
            break;
        } else if list.as_slice()[mid_index] < key {
            min_index = mid_index + 1;
        } else {
            max_index = mid_index;
        }
        if max_index <= min_index { break }
    }

    (found, mid_index)
}

