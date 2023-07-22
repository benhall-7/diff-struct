# Diff

A Rust trait for diffing and applying diffs between data structures. Features a derive macro.

Abstractly speaking, a 'diff' between two structs `A` and `B` can be considered the change necessary to make to `A` to produce `B`. Succinctly, the pattern can be written as `A -> B = D and A <- D = B`

Diff is automatically derived on several common base types, include: integers (i8, u32, etc), floats, non-zero integers, booleans, char, String, &str, and PathBuf.

Diff is also derived on several container types, such as: Option, Box, Rc, Arc, HashMap, BTreeMap, HashSet, BTreeSet, tuples (up to length 18), arrays, and Vec's.

Container types are subject to some type restrictions in order to diff. The inner type T always has to implement Diff. Depending on the container type, other restrictions apply, such as Debug, Clone, PartialEq. These restrictions may also apply to the diff type (T::Repr). The implementation of diffing Vec's is non-standard. It is faster and simpler than Myer's algorithm, but more error-prone on lists with many nearby duplicate elements.

## Derive macro

The derive macro can be used on tuple or field structs to produce a new struct representing their difference. Note that this new struct is not considered to be the same type as the base struct, because certain data types cannot represent their own diff (bools, maps, lists, etc). Currently only 1 helper attribute is supported, `attr` which allows passing attributes (doc comments/derives) to the generated struct. The generated struct name is by default the name of the base struct plus "Diff". Example:

```rust
#[derive(Debug, Default, PartialEq, Diff)]
// this will apply the specified derives on the generated 'ProjectMetaDiff' struct
#[diff(attr(
    #[derive(Debug, PartialEq)]
))]
pub struct ProjectMeta {
    contributors: Vec<String>,
    combined_work_hours: usize,
}

#[test]
fn test_apply() {
    let mut base = ProjectMeta::default();
    let contribution_a = ProjectMeta {
        contributors: vec!["Alice".into()],
        combined_work_hours: 3,
    };
    let contribution_b = ProjectMeta {
        contributors: vec!["Bob".into(), "Candice".into()],
        combined_work_hours: 10,
    };
    let expected = ProjectMeta {
        contributors: vec!["Bob".into(), "Candice".into(), "Alice".into()],
        combined_work_hours: 13,
    };
    let diff_a = base.diff(&contribution_a);
    let diff_b = base.diff(&contribution_b);
    base.apply(&diff_a);
    base.apply(&diff_b);
    assert_eq!(base, expected);
}
```

## In action

* [motion_lib](https://github.com/ultimate-research/motion_lib): a modding tool for the proprietary motion_lib.bin filetype in SSBU. Utilizing this crate, the project features functions for producing file differences as serialized yaml. With this functionality, users can determine quickly what changes were made to the regular file across game updates, and apply those changes to their modded files to keep them up to date. Vise versa, they can apply their modded file's diffs to the new version of the file to achieve the same result.

## In theory

An object implementing Diff will translate well into human-readable diffs of arbitrary data types, avoiding the problems and false positives of diffing or patching plain-text versions of the serialized structs.
