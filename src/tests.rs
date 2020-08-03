use super::{Diff, HashMapDiff};

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
fn map_diffs() {
    let a = generate_map(vec![("a", 1), ("b", 2)]);
    let b = generate_map(vec![("b", 3), ("c", 4)]);
    let expected = HashMapDiff {
        altered: generate_map(vec![("b", 1), ("c", 4)]),
        removed: vec!["a"].into_iter().collect::<HashSet<_>>(),
    };
    assert_eq!(a.diff(&b), expected);
    identity_test(a);
}

#[derive(Debug, PartialEq, Diff)]
pub struct TestStruct {
    a: bool,
    b: u32,
}

#[test]
fn test_derive() {
    let a = TestStruct {
        a: false,
        b: 42
    };

    let b = TestStruct {
        a: true,
        b: 43
    };

    let diff = TestStructDiff {
        a: true.into(),
        b: 1
    };
    assert_eq!(a.diff(&b), diff);

    identity_test(a);
}
