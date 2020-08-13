use super::*;

use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

fn identity_test<D: Diff + Debug + PartialEq>(s: D) {
    assert_eq!(D::identity().apply_new(&D::identity().diff(&s)), s);
}

fn generate_map<K: Eq + Hash, V>(parts: Vec<(K, V)>) -> HashMap<K, V> {
    parts.into_iter().collect::<HashMap<_, _>>()
}

#[test]
fn numeric_diffs() {
    identity_test(true);
    identity_test(42_u8);
    identity_test(42_i8);
    identity_test(42_u16);
    identity_test(42_i16);
    identity_test(42_u32);
    identity_test(42_i32);
    identity_test(42.0_f32);
    identity_test(42.0_f64);
}

#[test]
fn test_opt() {
    assert_eq!(Some(10).diff(&Some(15)), OptionDiff::Some(5));
    assert_eq!(None.apply_new(&OptionDiff::Some(5)), Some(5));
    assert_eq!(Some(100).apply_new(&OptionDiff::None), None);
    identity_test(Some(42))
}

#[test]
fn test_maps() {
    let a = generate_map(vec![("a", 1), ("b", 2), ("x", 42)]);
    let b = generate_map(vec![("b", 3), ("c", 4), ("x", 42)]);
    let expected = HashMapDiff {
        altered: generate_map(vec![("b", 1), ("c", 4)]),
        removed: vec!["a"].into_iter().collect::<HashSet<_>>(),
    };
    assert_eq!(a.diff(&b), expected);
    identity_test(a);
}

#[derive(Debug, PartialEq, Diff)]
#[diff(attr(
    #[derive(Debug, PartialEq)]
))]
struct TestStruct {
    a: bool,
    b: u32,
}

#[test]
fn test_derive() {
    let a = TestStruct { a: false, b: 42 };

    let b = TestStruct { a: true, b: 43 };

    let diff = TestStructDiff {
        a: true.into(),
        b: 1,
    };
    assert_eq!(a.diff(&b), diff);

    identity_test(a);
}

#[test]
fn test_vecs() {
    let a = vec![0, 1, 2, 3, 4, 5, 6, 7];
    let b = vec![0, /*1, 2*/ 3, 4, 42, 5, /*6 ->*/ 10, 7];
    let diff = VecDiff(vec![
        VecDiffType::Removed { index: 1, len: 2 },
        VecDiffType::Inserted {
            index: 5,
            changes: vec![42],
        },
        VecDiffType::Altered {
            index: 6,
            changes: vec![4], // add 4 to 6
        },
    ]);
    assert_eq!(diff, a.diff(&b));
    assert_eq!(a.apply_new(&diff), b);
}
