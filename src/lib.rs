
/// A trait to diff and apply diffs between two structs
pub trait Diff: Sized {
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolDiff {
    None,
    True,
    False,
}

impl From<bool> for BoolDiff {
    fn from(f: bool) -> Self {
        if f { BoolDiff::True } else { BoolDiff::False }
    }
}

impl Diff for bool {
    type Repr = BoolDiff;

    fn diff(&self, other: &Self) -> Self::Repr {
        if self != other {
            (*other).into()
        } else {
            BoolDiff::None
        }
    }

    fn apply_new(&self, diff: &Self::Repr) -> Self {
        match diff {
            BoolDiff::True => true,
            BoolDiff::False => false,
            BoolDiff::None => *self,
        }
    }
}

macro_rules! diff_int {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            fn diff(&self, other: &Self) -> Self::Repr {
                other.wrapping_sub(*self)
            }

            fn apply_new(&self, diff: &Self::Repr) -> Self {
                self.wrapping_add(*diff)
            }
        })*
    };
}

macro_rules! diff_float {($($ty:ty),*) => {
    $(impl Diff for $ty {
        type Repr = $ty;

        fn diff(&self, other: &Self) -> Self::Repr {
            other - self
        }

        fn apply_new(&self, diff: &Self::Repr) -> Self {
            self + diff
        }
    })*
};
}

diff_int!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);
diff_float!(f32, f64);