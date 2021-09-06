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

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub enum Enum {
    VarUnit,
    VarNamed{a: u32, b: u32},
    VarUnnamed(u32, u32),
}

fn tester<T>(mut a: T, b: T) -> bool
    where T: Diff + Eq
{
    let diff = a.diff(&b);
    a.apply(&diff);
    return a == b;
}

#[test]
fn test_basic() {
    quickcheck(tester as fn(Basic, Basic) -> bool);
}

#[test]
fn test_enum() {
    quickcheck(tester as fn(Enum, Enum) -> bool);
}
