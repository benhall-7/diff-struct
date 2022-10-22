#![allow(clippy::unused_unit)]

use diff::Diff;
use quickcheck::quickcheck;
use quickcheck_derive::Arbitrary;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct Unit;

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct Basic {
    pub items: HashMap<u16, Vec<u8>>,
    pub items_b: BTreeMap<u16, Vec<u8>>,
    pub tuple: (u32, u16),
    pub arc: Arc<String>,
    pub unit: Unit,
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub enum Enum {
    VarUnit,
    VarNamed { a: u32, b: u32 },
    VarUnnamed(u32, Basic),
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct NamedGenerics<A, B> {
    pub a: A,
    pub b: B,
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct UnnamedGenerics<A, B>(A, B);

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub enum EnumGenerics<A, B>
where
    A: PartialEq,
    B: PartialEq,
{
    Named { a: A, b: B },
    Unnamed(A, B),
}

pub trait Bound1 {}
pub trait Bound2 {}

impl Bound1 for u32 {}
impl Bound2 for u32 {}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct NamedGenericsBoundsInternal<A: Bound1 + Bound2, B> {
    pub a: A,
    pub b: B,
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct NamedGenericsBoundsExternal<A, B>
where
    A: Bound1 + Bound2,
{
    pub a: A,
    pub b: B,
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct NamedGenericsBoundsMixed<A: Bound1, B>
where
    A: Bound2,
{
    pub a: A,
    pub b: B,
}

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct UnnamedGenericsBoundsInternal<A: Bound1 + Bound2, B>(A, B);

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct UnnamedGenericsBoundsExternal<A, B>(A, B)
where
    A: Bound1 + Bound2;

#[derive(Clone, Arbitrary, Diff, Eq, PartialEq, Debug)]
pub struct UnnamedGenericsBoundsMixed<A: Bound1, B>(A, B)
where
    A: Bound2;

fn tester<T>(mut a: T, b: T) -> bool
where
    T: Diff + Eq,
{
    let diff = a.diff(&b);
    a.apply(&diff);
    a == b
}

#[test]
fn test_basic() {
    quickcheck(tester as fn(Basic, Basic) -> bool);
}

#[test]
fn test_named_generics() {
    quickcheck(tester as fn(NamedGenerics<u32, u64>, NamedGenerics<u32, u64>) -> bool);
}

#[test]
fn test_unnamed_generics() {
    quickcheck(tester as fn(UnnamedGenerics<u32, u64>, UnnamedGenerics<u32, u64>) -> bool);
}

#[test]
fn test_enum_generics() {
    quickcheck(tester as fn(EnumGenerics<u32, u64>, EnumGenerics<u32, u64>) -> bool);
}

#[test]
fn test_enum() {
    quickcheck(tester as fn(Enum, Enum) -> bool);
}

#[test]
fn test_named_generics_bounds_internal() {
    quickcheck(
        tester
            as fn(
                NamedGenericsBoundsInternal<u32, u64>,
                NamedGenericsBoundsInternal<u32, u64>,
            ) -> bool,
    );
}

#[test]
fn test_named_generics_bounds_external() {
    quickcheck(
        tester
            as fn(
                NamedGenericsBoundsExternal<u32, u64>,
                NamedGenericsBoundsExternal<u32, u64>,
            ) -> bool,
    );
}

#[test]
fn test_named_generics_bounds_mixed() {
    quickcheck(
        tester
            as fn(NamedGenericsBoundsMixed<u32, u64>, NamedGenericsBoundsMixed<u32, u64>) -> bool,
    );
}

#[test]
fn test_unnamed_generics_bounds_internal() {
    quickcheck(
        tester
            as fn(
                UnnamedGenericsBoundsInternal<u32, u64>,
                UnnamedGenericsBoundsInternal<u32, u64>,
            ) -> bool,
    );
}

#[test]
fn test_unnamed_generics_bounds_external() {
    quickcheck(
        tester
            as fn(
                UnnamedGenericsBoundsExternal<u32, u64>,
                UnnamedGenericsBoundsExternal<u32, u64>,
            ) -> bool,
    );
}

#[test]
fn test_unnamed_generics_bounds_mixed() {
    quickcheck(
        tester
            as fn(
                UnnamedGenericsBoundsMixed<u32, u64>,
                UnnamedGenericsBoundsMixed<u32, u64>,
            ) -> bool,
    );
}
