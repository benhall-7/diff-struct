use super::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BoolDiff {
    None,
    True,
    False,
}

impl From<bool> for BoolDiff {
    fn from(f: bool) -> Self {
        if f {
            BoolDiff::True
        } else {
            BoolDiff::False
        }
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

    fn identity() -> Self {
        false
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

            fn identity() -> $ty {
                0
            }
        })*
    };
}

macro_rules! diff_float {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            fn diff(&self, other: &Self) -> Self::Repr {
                other - self
            }

            fn apply_new(&self, diff: &Self::Repr) -> Self {
                self + diff
            }

            fn identity() -> $ty {
                0.0
            }
        })*
    };
}

diff_int!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);
diff_float!(f32, f64);

#[derive(Debug, PartialEq)]
pub enum OptionDiff<T>
where
    T: Diff,
{
    Some(T::Repr),
    None,
    NoChange
}

impl<T> Diff for Option<T>
where
    T: Diff,
{
    type Repr = OptionDiff<T>;

    fn diff(&self, other: &Self) -> Self::Repr {
        match (self, other) {
            (Some(value), Some(other_value)) => {
                if value == other_value {
                    OptionDiff::NoChange
                } else {
                    OptionDiff::Some(value.diff(other_value))
                }
            }
            (Some(_), None) => OptionDiff::None,
            (None, Some(other_value)) => {
                OptionDiff::Some(T::identity().diff(other_value))
            }
            (None, None) => OptionDiff::NoChange,
        }
    }

    fn apply_new(&self, diff: &Self::Repr) -> Self {
        let mut new = Self::identity();
        // basically get a new copy of self
        new.apply(&Self::identity().diff(self));
        new.apply(diff);
        new
    }

    fn apply(&mut self, diff: &Self::Repr) {
        match diff {
            OptionDiff::None => *self = None,
            OptionDiff::Some(diff_value) => {
                if let Some(value) = self {
                    value.apply(diff_value);
                } else {
                    T::identity().apply(diff_value);
                }
            }
            _ => {}
        }
    }

    fn identity() -> Self {
        None
    }
}

/// The diff struct used to compare two HashMap's
#[derive(Debug, PartialEq)]
pub struct HashMapDiff<K, V>
where
    K: Eq + Hash + Clone,
    V: Diff,
    <V as Diff>::Repr: Debug + PartialEq,
{
    /// Values that are changed or added
    pub altered: HashMap<K, <V as Diff>::Repr>,
    /// Values that are removed
    pub removed: HashSet<K>,
}

impl<K, V> Diff for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Diff,
    <V as Diff>::Repr: Debug + PartialEq,
{
    type Repr = HashMapDiff<K, V>;

    fn diff(&self, other: &Self) -> Self::Repr {
        let mut diff = HashMapDiff {
            altered: HashMap::new(),
            removed: HashSet::new(),
        };
        // can we do better than this?
        for (key, value) in self {
            if let Some(other_value) = other.get(key) {
                diff.altered.insert(key.clone(), value.diff(other_value));
            } else {
                diff.removed.insert(key.clone());
            }
        }
        for (key, value) in other {
            if let None = self.get(key) {
                diff.altered.insert(key.clone(), V::identity().diff(value));
            }
        }
        diff
    }

    // allocation makes this a bit expensive
    fn apply_new(&self, diff: &Self::Repr) -> Self {
        let mut new = Self::identity();
        // basically get a new copy of self
        new.apply(&Self::identity().diff(self));
        new.apply(diff);
        new
    }

    // basically inexpensive
    fn apply(&mut self, diff: &Self::Repr) {
        diff.removed.iter().for_each(|del| {
            self.remove(del);
        });
        for (key, change) in &diff.altered {
            if let Some(original) = self.get_mut(key) {
                original.apply(change);
            } else {
                self.insert(key.clone(), V::identity().apply_new(change));
            }
        }
    }

    fn identity() -> Self {
        HashMap::new()
    }
}
