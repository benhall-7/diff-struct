
/// A trait to diff and apply diffs between two structs
pub trait Diff {
    /// The type associated with the structs' difference
    type Repr;

    /// Produces a diff between two structs
    fn diff(&self, other: &Self) -> Self::Repr;
    /// Applies the diff to the struct and produces a new struct
    fn apply_new(&self, diff: &Self::Repr) -> Self;
    /// Applies the diff directly to the struct
    fn apply(&mut self, diff: &Self::Repr) {
        self = self.apply_new(diff);
    }
}