//! Extremely opinionated testing framework producing an exact specification alongside an almost provably optimal implementation.

#![warn(clippy::missing_docs_in_private_items)]
#![deny(warnings)]

#[macro_use]
mod util;

pub use poirot_mutator::*;

fn main() -> anyhow::Result<()> {
    Ok(())
}

#[poirot(does_nothing(&0), does_nothing(&1), does_something(&0))]
fn id(a: &u8) -> &u8 {
    a
}

fn does_nothing<T: PartialEq, F: Fn(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) == x
}

fn does_something<T: PartialEq, F: Fn(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) != x
}

#[test]
fn id_automatic_test() {
    let _: Option<()> = id_check_cover(&id).and_then(|e| panic!("{}", e));
}
