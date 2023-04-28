//! Extremely opinionated testing framework producing an exact specification alongside an almost provably optimal implementation.

#![warn(clippy::missing_docs_in_private_items)]
#![deny(warnings)]

pub use poirot_mutator::*;

fn main() -> anyhow::Result<()> {
    assert!(does_nothing(id, &0));
    id_check_cover(&id);

    Ok(())
}

#[poirot(does_nothing(&0), does_nothing(&1))]
fn id(a: &u8) -> &u8 {
    a
}

fn does_nothing<T: PartialEq, F: Fn(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) == x
}
