use super::utils::find_match;
use super::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

impl Diff for bool {
    type Repr = Option<bool>;

    fn diff(&self, other: &Self) -> Self::Repr {
        if self != other {
            Some(*other)
        } else {
            None
        }
    }

    fn apply(&mut self, diff: &Self::Repr) {
        if let Some(diff) = diff {
            *self = *diff;
        }
    }

    fn identity() -> Self {
        false
    }
}

impl<T> Diff for Arc<T>
where
    T: Diff + Clone,
{
    type Repr = T::Repr;

    fn diff(&self, other: &Self) -> Self::Repr {
        self.deref().diff(other.deref())
    }

    fn apply(&mut self, diff: &Self::Repr) {
        match Arc::get_mut(self) {
            Some(m) => m.apply(diff),
            None => {
                let mut x = (**self).clone();
                x.apply(diff);
                *self = Arc::new(x);
            }
        }
    }

    fn identity() -> Self {
        Arc::new(T::identity())
    }
}

macro_rules! diff_tuple {
    (($($ty:ident),*), ($($access:tt),*)) => {
        impl<$($ty),*> Diff for ($($ty),*,)
            where $($ty: Diff),*
        {
            type Repr = ($(<$ty>::Repr),*,);

            fn diff(&self, other: &Self) -> Self::Repr {
                ($(self.$access.diff(&other.$access)),*,)
            }

            fn apply(&mut self, diff: &Self::Repr) {
                $(self.$access.apply(&diff.$access));*;
            }

            fn identity() -> Self {
                ($(<$ty>::identity()),*,)
            }
        }
    }
}

diff_tuple!((A), (0));
diff_tuple!((A, B), (0, 1));
diff_tuple!((A, B, C), (0, 1, 2));
diff_tuple!((A, B, C, D), (0, 1, 2, 3));
diff_tuple!((A, B, C, D, F), (0, 1, 2, 3, 4));
diff_tuple!((A, B, C, D, F, G), (0, 1, 2, 3, 4, 5));
diff_tuple!((A, B, C, D, F, G, H), (0, 1, 2, 3, 4, 5, 6));
diff_tuple!((A, B, C, D, F, G, H, I), (0, 1, 2, 3, 4, 5, 6, 7));
diff_tuple!((A, B, C, D, F, G, H, I, J), (0, 1, 2, 3, 4, 5, 6, 7, 8));
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
);
diff_tuple!(
    (A, B, C, D, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
);

macro_rules! diff_int {
    ($($ty:ty),*) => {
        $(impl Diff for $ty {
            type Repr = $ty;

            fn diff(&self, other: &Self) -> Self::Repr {
                other.wrapping_sub(*self)
            }

            fn apply(&mut self, diff: &Self::Repr) {
                *self = self.wrapping_add(*diff);
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

            fn apply(&mut self, diff: &Self::Repr){
                *self += diff;
            }

            fn identity() -> $ty {
                0.0
            }
        })*
    };
}

diff_int!(u8, i8, u16, i16, u32, i32, u64, i64, usize, isize);
diff_float!(f32, f64);

impl Diff for char {
    type Repr = Option<char>;

    fn diff(&self, other: &Self) -> Self::Repr {
        if self != other {
            Some(*other)
        } else {
            None
        }
    }

    fn apply(&mut self, diff: &Self::Repr) {
        if let Some(diff) = diff {
            *self = *diff
        }
    }

    fn identity() -> Self {
        '\x00'
    }
}

impl Diff for String {
    type Repr = Option<String>;

    fn diff(&self, other: &Self) -> Self::Repr {
        if self != other {
            Some(other.clone())
        } else {
            None
        }
    }

    fn apply(&mut self, diff: &Self::Repr) {
        if let Some(diff) = diff {
            *self = diff.clone()
        }
    }

    fn identity() -> Self {
        String::new()
    }
}

#[derive(Serialize, Deserialize)]
pub enum OptionDiff<T: Diff> {
    Some(T::Repr),
    New(T),
    None,
    Same,
}

// None -> None = NoChange
// None -> Some = New(T)
// Some -> None = None
// Some -> Some = either NoChange or Some(T::Diff)

impl<T: Diff + PartialEq> Diff for Option<T> {
    type Repr = OptionDiff<T>;

    fn diff(&self, other: &Self) -> Self::Repr {
        match (self, other) {
            (Some(value), Some(other_value)) => {
                if value == other_value {
                    OptionDiff::Same
                } else {
                    OptionDiff::Some(value.diff(other_value))
                }
            }
            (Some(_), None) => OptionDiff::None,
            (None, Some(other_value)) => {
                OptionDiff::New(T::identity().apply_new(&T::identity().diff(other_value)))
            }
            (None, None) => OptionDiff::Same,
        }
    }

    fn apply(&mut self, diff: &Self::Repr) {
        match diff {
            OptionDiff::None => *self = None,
            OptionDiff::Some(change) => {
                if let Some(value) = self {
                    value.apply(change);
                } else {
                    *self = Some(T::identity().apply_new(change))
                }
            }
            _ => {}
        }
    }

    fn identity() -> Self {
        None
    }
}

impl<T: Diff> Debug for OptionDiff<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match &self {
            OptionDiff::Some(change) => f.debug_tuple("Some").field(change).finish(),
            OptionDiff::None => write!(f, "None"),
            OptionDiff::Same => write!(f, "NoChange"),
        }
    }
}

impl<T: Diff> PartialEq for OptionDiff<T>
where
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (OptionDiff::Some(a), OptionDiff::Some(b)) => a == b,
            (OptionDiff::None, OptionDiff::None) => true,
            (OptionDiff::Same, OptionDiff::Same) => true,
            _ => false,
        }
    }
}

impl<T: Diff> Clone for OptionDiff<T>
where
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        match self {
            OptionDiff::Some(a) => OptionDiff::Some(a.clone()),
            OptionDiff::None => OptionDiff::None,
            OptionDiff::Same => OptionDiff::Same,
        }
    }
}

/// The diff struct used to compare two [HashMap]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "V::Repr: Serialize, K: Serialize"))]
#[serde(bound(deserialize = "V::Repr: Deserialize<'de>, K: Deserialize<'de>"))]
pub struct HashMapDiff<K: Hash + Eq, V: Diff> {
    /// Values that are changed or added
    pub altered: HashMap<K, <V as Diff>::Repr>,
    /// Values that are removed
    pub removed: HashSet<K>,
}

/// The diff struct used to compare two [BTreeMap]'s
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "V::Repr: Serialize, K: Serialize"))]
#[serde(bound(deserialize = "V::Repr: Deserialize<'de>, K: Deserialize<'de>"))]
pub struct BTreeMapDiff<K: Ord + Eq, V: Diff> {
    /// Values that are changed or added
    pub altered: BTreeMap<K, <V as Diff>::Repr>,
    /// Values that are removed
    pub removed: BTreeSet<K>,
}

macro_rules! diff_map {
    ($ty: ident, $diffty: ident, $diffkey: ident, ($($constraints:tt)*)) => {
        impl<K: $($constraints)*, V: Diff> Diff for $ty<K, V>
        where
            K: Clone,
            V: PartialEq,
        {
            type Repr = $diffty<K, V>;

            fn diff(&self, other: &Self) -> Self::Repr {
                let mut diff = $diffty {
                    altered: $ty::new(),
                    removed: $diffkey::new(),
                };
                // can we do better than this?
                for (key, value) in self {
                    if let Some(other_value) = other.get(key) {
                        // don't store values that don't change
                        if value != other_value {
                            diff.altered.insert(key.clone(), value.diff(other_value));
                        }
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
                $ty::new()
            }
        }

        impl<K: $($constraints)*, V: Diff> Debug for $diffty<K, V>
        where
            K: Debug,
            V::Repr: Debug,
        {
            fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                f.debug_struct(stringify!($diffty))
                    .field("altered", &self.altered)
                    .field("removed", &self.removed)
                    .finish()
            }
        }

        impl<K: $($constraints)*, V: Diff> PartialEq for $diffty<K, V>
        where
            V::Repr: PartialEq,
        {
            fn eq(&self, other: &Self) -> bool {
                self.altered == other.altered && self.removed == other.removed
            }
        }

        impl<K: $($constraints)*, V: Diff> Clone for $diffty<K, V>
        where
            K: Clone,
            V::Repr: Clone,
        {
            fn clone(&self) -> Self {
                $diffty {
                    altered: self.altered.clone(),
                    removed: self.removed.clone(),
                }
            }
        }
    }
}

diff_map!(HashMap, HashMapDiff, HashSet, (Hash + Eq));
diff_map!(BTreeMap, BTreeMapDiff, BTreeSet, (Ord));

/// The type of change to make to a vec
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Repr: Serialize"))]
#[serde(bound(deserialize = "T::Repr: Deserialize<'de>"))]
pub enum VecDiffType<T: Diff> {
    Removed { index: usize, len: usize },
    Altered { index: usize, changes: Vec<T::Repr> },
    Inserted { index: usize, changes: Vec<T::Repr> },
}

impl<T: Diff> Debug for VecDiffType<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            VecDiffType::Removed { index, len } => f
                .debug_struct("Removed")
                .field("index", index)
                .field("len", len)
                .finish(),
            VecDiffType::Altered { index, changes } => f
                .debug_struct("Altered")
                .field("index", index)
                .field("changes", changes)
                .finish(),
            VecDiffType::Inserted { index, changes } => f
                .debug_struct("Inserted")
                .field("index", index)
                .field("changes", changes)
                .finish(),
        }
    }
}

impl<T: Diff> PartialEq for VecDiffType<T>
where
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                VecDiffType::Removed { index, len },
                VecDiffType::Removed {
                    index: ref index_,
                    len: ref len_,
                },
            ) => index == index_ && len == len_,
            (
                VecDiffType::Altered { index, changes },
                VecDiffType::Altered {
                    index: ref index_,
                    changes: ref changes_,
                },
            ) => index == index_ && changes == changes_,
            (
                VecDiffType::Inserted { index, changes },
                VecDiffType::Inserted {
                    index: ref index_,
                    changes: ref changes_,
                },
            ) => index == index_ && changes == changes_,
            _ => false,
        }
    }
}

impl<T: Diff> Clone for VecDiffType<T>
where
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        match self {
            VecDiffType::Removed { index, len } => VecDiffType::Removed {
                index: *index,
                len: *len,
            },
            VecDiffType::Altered { index, changes } => VecDiffType::Altered {
                index: *index,
                changes: changes.clone(),
            },
            VecDiffType::Inserted { index, changes } => VecDiffType::Inserted {
                index: *index,
                changes: changes.clone(),
            },
        }
    }
}

/// The collection of difference-vec's
#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Repr: Serialize"))]
#[serde(bound(deserialize = "T::Repr: Deserialize<'de>"))]
pub struct VecDiff<T: Diff>(pub Vec<VecDiffType<T>>);

impl<T: Diff + PartialEq> Diff for Vec<T> {
    type Repr = VecDiff<T>;

    fn diff(&self, other: &Self) -> Self::Repr {
        let mut changes = Vec::new();
        let mut pos_x = 0;
        let mut pos_y = 0;
        loop {
            let (is_match, deletions, insertions) = find_match(&self[pos_x..], &other[pos_y..]);

            // TODO: simplify logic here
            if deletions == 0 || insertions == 0 {
                if deletions > 0 {
                    changes.push(VecDiffType::Removed {
                        index: pos_x,
                        len: deletions,
                    });
                } else if insertions > 0 {
                    changes.push(VecDiffType::Inserted {
                        index: pos_x,
                        changes: other[pos_y..pos_y + insertions]
                            .iter()
                            .map(|new| T::identity().diff(new))
                            .collect(),
                    });
                }
            } else if deletions == insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + deletions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(b))
                        .collect(),
                });
            } else if deletions > insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + insertions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(b))
                        .collect(),
                });
                changes.push(VecDiffType::Removed {
                    index: pos_x + insertions,
                    len: deletions - insertions,
                });
            } else {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + deletions]
                        .iter()
                        .zip(other[pos_y..pos_y + deletions].iter())
                        .map(|(a, b)| a.diff(b))
                        .collect(),
                });
                changes.push(VecDiffType::Inserted {
                    index: pos_x + deletions,
                    changes: other[pos_y + deletions..pos_y + insertions]
                        .iter()
                        .map(|new| T::identity().diff(new))
                        .collect(),
                });
            }

            if is_match {
                pos_x += deletions + 1;
                pos_y += insertions + 1;
            } else {
                break;
            }
        }
        VecDiff(changes)
    }

    fn apply(&mut self, diff: &Self::Repr) {
        let mut relative_index = 0_isize;
        for change in &diff.0 {
            match change {
                VecDiffType::Removed { index, len } => {
                    let index = (*index as isize + relative_index) as usize;
                    self.drain(index..index + len);
                    relative_index -= *len as isize;
                }
                VecDiffType::Inserted { index, changes } => {
                    let index = (*index as isize + relative_index) as usize;
                    self.splice(
                        index..index,
                        changes.iter().map(|d| T::identity().apply_new(d)),
                    );
                    relative_index += changes.len() as isize;
                }
                VecDiffType::Altered { index, changes } => {
                    let index = (*index as isize + relative_index) as usize;
                    let range = index..index + changes.len();
                    for (value, diff) in self[range].iter_mut().zip(changes.iter()) {
                        value.apply(diff);
                    }
                }
            }
        }
    }

    fn identity() -> Self {
        Vec::new()
    }
}

impl<T: Diff> Debug for VecDiff<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl<T: Diff> PartialEq for VecDiff<T>
where
    T::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Diff> Clone for VecDiff<T>
where
    T::Repr: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
