use super::{Diff, HashMapDiff};

use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

fn identity_test<D: Diff + Debug + PartialEq>(s: D) {
    assert_eq!(
        D::identity().apply_new(&D::identity().diff(&s)),
        s
    );
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
    let a = vec![
        ("a", 1),
        ("b", 2),
    ].into_iter().collect::<HashMap<_,_>>();
    let b = vec![
        ("b", 3),
        ("c", 4),
    ].into_iter().collect::<HashMap<_,_>>();
    let expected = HashMapDiff {
        altered: vec![("b", 1), ("c", 4)].into_iter().collect::<HashMap<_,_>>(),
        removed: vec!["a"].into_iter().collect::<HashSet<_>>(),
    };
    assert_eq!(a.diff(&b), expected);
    identity_test(a);
}