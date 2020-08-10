use std::cmp::{max, min};

/// Finds the closest-to-starting element present in both slices.
/// Returns whether an equal element was found in each slice, the index of the element in a, the index in b.
/// If no match was found between a and b, returns the indeces of the end of each slice, respectively.
pub fn find_match<T: PartialEq>(a: &[T], b: &[T]) -> (bool, usize, usize) {
    let (mut x, mut y) = (0, 0);
    let mut found_match = false;
    if !a.is_empty() && !b.is_empty() {
        let max_depth = a.len() + b.len() - 1;
        for depth in 0..max_depth {
            let x_lower_bound = max(depth as isize - b.len() as isize + 1, 0) as usize;
            x = min(depth, a.len() - 1);
            loop {
                y = depth - x;
                if a[x] == b[y] {
                    found_match = true;
                    break;
                }
                if x > x_lower_bound {
                    x -= 1;
                } else {
                    break;
                }
            }

            if found_match {
                break;
            }
        }
    }
    if !found_match {
        x = a.len();
        y = b.len();
    }
    (found_match, x, y)
}
