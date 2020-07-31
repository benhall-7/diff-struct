use super::Diff;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::fmt::Debug;

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

    const Identity: BoolDiff = BoolDiff::None;

    fn diff(&self, other: &Self) -> Self::Repr {
        if self != other {
            (*other).into()
        } else {
            Self::Identity
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

            const Identity: $ty = 0;

            fn diff(&self, other: &Self) -> Self::Repr {
                other.wrapping_sub(*self)
            }

            fn apply_new(&self, diff: &Self::Repr) -> Self {
                self.wrapping_add(*diff)
            }
        })*
    };
}

macro_rules! diff_float {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            const Identity: $ty = 0.0;

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

/// The diff struct used to compare two HashMap's
#[derive(Debug)]
pub struct HashMapDiff<K, V>
where
    K: Eq + Hash + Clone,
    V: Diff,
    <V as Diff>::Repr: Debug
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
    <V as Diff>::Repr: Debug
{
    type Repr = Option<HashMapDiff<K, V>>;

    const Identity: Self::Repr = None;

    fn diff(&self, other: &Self) -> Self::Repr {
        let mut diff = HashMapDiff {
            altered: HashMap::new(),
            removed: HashSet::new(),
        };
        for (key, value) in self.into_iter() {
            if let Some(other_value) = other.get(key) {
                diff.altered.insert(key.clone(), value.diff(other_value));
            } else {
                diff.removed.insert(key.clone());
            }
        }
        Some(diff)
    }

    // more expensive operation due to extra allocation
    fn apply_new(&self, diff: &Self::Repr) -> Self {
        if let Some(diff) = diff {
            let mut new = HashMap::new();
            for (key, value) in self {
                if !diff.removed.contains(key) {
                    if let Some(change) = diff.altered.get(key) {
                        new.insert(key.clone(), value.apply_new(change));
                    } else {
                        new.insert(key.clone(), value.apply_new(&V::Identity));
                    }
                }
            }
            new
        } else {
            self.into_iter()
                .map(|(key, value)| (key.clone(), value.apply_new(&V::Identity)))
                .collect::<HashMap<_, _>>()
        }
    }

    fn apply(&mut self, diff: &Self::Repr) {
        if let Some(diff) = diff {
            diff.removed.iter().for_each(|del| {
                self.remove(del);
            });
            for (key, value) in self {
                if let Some(change) = diff.altered.get(key) {
                    value.apply(change);
                }
            }
        }
    }
}

// {
//   "a": "test a",
//   "b": "test b"
// }
// -->
// {
//   "a": "test a",
//   "b": "test C!!!"
// }
// diff =
// { "b": "test C!!!" }
//
// ANOTHER EXAMPLE
//
// {
//   "a": "test a",
//   "b": "test b"
// }
// -->
// {
//   "a": "test a",
//   "c": "test b"
// }
// diff =
// {
//   "b": removed?
//   "c": "test b"
// }
