trait DefaultDiff: Sized {
    type Method: DiffMethod<Self>;

    fn diff(&self, other: &Self) -> <Self::Method as DiffMethod<Self>>::Repr {
        <Self::Method as DiffMethod<Self>>::diff(self, other)
    }

    fn patch(&mut self, diff: &<Self::Method as DiffMethod<Self>>::Repr) {
        <Self::Method as DiffMethod<Self>>::patch(self, diff)
    }
}

trait Diff: DefaultDiff {
    fn diff2<M: DiffMethod<Self>>(&self, other: &Self) -> M::Repr {
        M::diff(self, other)
    }

    fn patch2<M: DiffMethod<Self>>(&mut self, diff: &M::Repr) {
        M::patch(self, diff)
    }

    // fn identity() -> Self;
}

trait DiffMethod<T> {
    type Repr;

    fn diff(a: &T, b: &T) -> Self::Repr;

    fn patch(a: &mut T, diff: &Self::Repr);
}

struct EqualityDiffer;

impl<T> DiffMethod<T> for EqualityDiffer
where
    T: PartialEq + Clone,
{
    type Repr = Option<T>;

    fn diff(a: &T, b: &T) -> Self::Repr {
        (a != b).then(|| b.clone())
    }

    fn patch(a: &mut T, diff: &Self::Repr) {
        if let Some(diff) = diff {
            *a = diff.clone();
        }
    }
}

// auto-implemented
impl<T: DefaultDiff> Diff for T {}

impl DefaultDiff for i32 {
    type Method = EqualityDiffer;
}

impl DefaultDiff for String {
    type Method = EqualityDiffer;
}

#[test]
fn demonstration() {
    let mut value = String::from("initial");
    let value2 = String::from("new");
    let diff = value.diff(&value2);
    assert_eq!(diff, Some(String::from("new")));

    value.patch(&diff);
    assert_eq!(value, String::from("new"));
}
