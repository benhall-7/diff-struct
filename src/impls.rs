use super::utils::find_match;
use super::*;
use serde::{Serialize, Serializer};
use serde::ser::{SerializeStruct, SerializeStructVariant};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::hash::Hash;

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

pub enum OptionDiff<T: Diff> {
    Some(T::Repr),
    None,
    NoChange,
}

impl<T: Diff + PartialEq> Diff for Option<T> {
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
            (None, Some(other_value)) => OptionDiff::Some(T::identity().diff(other_value)),
            (None, None) => OptionDiff::NoChange,
        }
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

impl<T: Diff> Debug for OptionDiff<T>
where
    T::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match &self {
            OptionDiff::Some(change) => f.debug_tuple("Some").field(change).finish(),
            OptionDiff::None => write!(f, "None"),
            OptionDiff::NoChange => write!(f, "NoChange"),
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
            (OptionDiff::NoChange, OptionDiff::NoChange) => true,
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
            OptionDiff::NoChange => OptionDiff::NoChange,
        }
    }
}

impl<T: Diff> Serialize for OptionDiff<T>
where
    T::Repr: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            OptionDiff::Some(c) => serializer.serialize_newtype_variant("OptionDiff", 0, "Some", c),
            OptionDiff::None => serializer.serialize_unit_variant("OptionDiff", 1, "None"),
            OptionDiff::NoChange => serializer.serialize_unit_variant("OptionDiff", 2, "NoChange"),
        }
    }
}

/// The diff struct used to compare two HashMap's
pub struct HashMapDiff<K: Hash + Eq, V: Diff> {
    /// Values that are changed or added
    pub altered: HashMap<K, <V as Diff>::Repr>,
    /// Values that are removed
    pub removed: HashSet<K>,
}

impl<K: Hash + Eq, V: Diff + PartialEq> Diff for HashMap<K, V>
where
    K: Clone,
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

impl<K: Eq + Hash, V: Diff> Debug for HashMapDiff<K, V>
where
    K: Debug,
    V::Repr: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("HashMapDiff")
            .field("altered", &self.altered)
            .field("removed", &self.removed)
            .finish()
    }
}

impl<K: Eq + Hash, V: Diff> PartialEq for HashMapDiff<K, V>
where
    V::Repr: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.altered == other.altered && self.removed == other.removed
    }
}

impl<K: Eq + Hash, V: Diff> Clone for HashMapDiff<K, V>
where
    K: Clone,
    V::Repr: Clone,
{
    fn clone(&self) -> Self {
        HashMapDiff {
            altered: self.altered.clone(),
            removed: self.removed.clone(),
        }
    }
}

impl<K: Eq + Hash, V: Diff> Serialize for HashMapDiff<K, V>
where
    K: Serialize,
    V::Repr: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut start = serializer.serialize_struct("HashMapDiff", 2)?;
        start.serialize_field("altered", &self.altered)?;
        start.serialize_field("removed", &self.removed)?;
        start.end()
    }
}

/// The type of change to make to a vec
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
                .debug_struct("Removed")
                .field("index", index)
                .field("changes", changes)
                .finish(),
            VecDiffType::Inserted { index, changes } => f
                .debug_struct("Removed")
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
    T::Repr: Clone
{
    fn clone(&self) -> Self {
        match self {
            VecDiffType::Removed { index, len } => {
                VecDiffType::Removed { index: *index, len: *len }
            }
            VecDiffType::Altered { index, changes } => {
                VecDiffType::Altered { index: *index, changes: changes.clone() }
            }
            VecDiffType::Inserted { index, changes } => {
                VecDiffType::Inserted { index: *index, changes: changes.clone() }
            }
        }
    }
}

impl<T: Diff> Serialize for VecDiffType<T>
where
    T::Repr: Serialize
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        match self {
            VecDiffType::Removed { index, len } => {
                let mut start = serializer.serialize_struct_variant("VecDiffType", 0, "Removed", 2)?;
                start.serialize_field("index", index)?;
                start.serialize_field("len", len)?;
                start.end()
            }
            VecDiffType::Altered { index, changes } => {
                let mut start = serializer.serialize_struct_variant("VecDiffType", 0, "Removed", 2)?;
                start.serialize_field("index", index)?;
                start.serialize_field("changes", changes)?;
                start.end()
            }
            VecDiffType::Inserted { index, changes } => {
                let mut start = serializer.serialize_struct_variant("VecDiffType", 0, "Removed", 2)?;
                start.serialize_field("index", index)?;
                start.serialize_field("changes", changes)?;
                start.end()
            }
        }
    }
}

/// The collection of difference-vec's
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
                            .map(|new| T::identity().diff(&new))
                            .collect(),
                    });
                }
            } else if deletions == insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + deletions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(&b))
                        .collect(),
                });
            } else if deletions > insertions {
                changes.push(VecDiffType::Altered {
                    index: pos_x,
                    changes: self[pos_x..pos_x + insertions]
                        .iter()
                        .zip(other[pos_y..pos_y + insertions].iter())
                        .map(|(a, b)| a.diff(&b))
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
                        .map(|(a, b)| a.diff(&b))
                        .collect(),
                });
                changes.push(VecDiffType::Inserted {
                    index: pos_x + deletions,
                    changes: other[pos_y + deletions..pos_y + insertions]
                        .iter()
                        .map(|new| T::identity().diff(&new))
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

    fn apply(&mut self, _diff: &Self::Repr) {
        todo!();
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
    T::Repr: PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Diff> Clone for VecDiff<T>
where
    T::Repr: Clone
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Diff> Serialize for VecDiff<T>
where
    T::Repr: Serialize
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_newtype_struct("VecDiff", &self.0)    
    }
}
