use quickcheck::quickcheck;
use quickcheck_derive::Arbitrary;
use std::collections::HashMap;
use diff::Diff;

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct Unit;

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct Basic {
    pub items: HashMap<u16, Vec<u8>>,
    pub unit: Unit,
}

fn tester<T>(mut a: T, b: T) -> bool
    where T: Diff + Eq
{
    let diff = a.diff(&b);
    a.apply(&diff);
    return a == b;
}

#[test]
fn test() {
    quickcheck(tester as fn(Basic, Basic) -> bool);
}
