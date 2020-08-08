mod impls;
#[cfg(test)]
mod tests;

pub use diff_derive::Diff;
pub use impls::*;

/// A trait to diff and apply diffs between two structs
/// The derive macro can be used on structs when all fields of the struct implement Diff
/// Implementations are provided for bools, numeric types, Option types, and HashMaps
pub trait Diff: Sized {
    /// The type associated with the structs' difference
    type Repr;

    /// Produces a diff between two structs
    fn diff(&self, other: &Self) -> Self::Repr;
    /// Applies the diff directly to the struct
    fn apply(&mut self, diff: &Self::Repr);
    /// The identity element of the struct
    /// ```
    /// use diff::Diff;
    /// let s = 42;
    /// let i = <i32 as Diff>::identity();
    /// assert_eq!(i.apply_new(&i.diff(&s)), s);
    /// ```
    /// or mathematically speaking, `i + (s - i) = s`
    fn identity() -> Self;
    /// Applies the diff to the struct and produces a new struct
    fn apply_new(&self, diff: &Self::Repr) -> Self {
        let mut new = Self::identity();
        new.apply(&new.diff(self));
        new.apply(diff);
        new
    }
}
