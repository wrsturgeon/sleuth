//! Extremely opinionated testing framework producing an exact specification alongside an almost provably optimal implementation.

#![deny(warnings, missing_docs)]

pub use poirot_mutator::*;

fn main() -> anyhow::Result<()> {
    assert!(does_nothing(id, &0));

    Ok(())
}

#[poirot(does_nothing(&0), does_nothing(&1))]
fn id<A>(a: A) -> A {
    a
}

fn does_nothing<T: PartialEq, F: FnOnce(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) == x
}
