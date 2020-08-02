mod impls;
#[cfg(test)]
mod tests;

pub use impls::*;

/// A trait to diff and apply diffs between two structs
pub trait Diff: Sized + PartialEq {
    /// The type associated with the structs' difference
    type Repr;

    /// Produces a diff between two structs
    fn diff(&self, other: &Self) -> Self::Repr;
    /// Applies the diff to the struct and produces a new struct
    fn apply_new(&self, diff: &Self::Repr) -> Self;
    /// Applies the diff directly to the struct. This function is auto-implemented
    fn apply(&mut self, diff: &Self::Repr) {
        *self = self.apply_new(diff);
    }
    /// The identity element of the struct
    /// ```
    /// use diff::Diff;
    /// let s = 42;
    /// let i = <i32 as Diff>::identity();
    /// assert_eq!(i.apply_new(&i.diff(&s)), s);
    /// ```
    /// or mathematically speaking, `i + (s - i) = s`
    fn identity() -> Self;
}
