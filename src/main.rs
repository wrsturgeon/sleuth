//! Extremely opinionated testing framework producing an exact specification alongside an almost provably optimal implementation.

#![warn(
    missing_docs,
    missing_copy_implementations,
    missing_debug_implementations,
    rustdoc::all,
    clippy::missing_docs_in_private_items,
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]
#![allow(clippy::blanket_clippy_restriction_lints, clippy::implicit_return)]
#![deny(warnings)]

mod util;

#[cfg(test)]
mod poirot;

#[allow(clippy::pub_use)]
pub use poirot_mutator::*;

fn main() -> anyhow::Result<()> {
    Ok(())
}

#[poirot(does_nothing(&0), does_nothing(&1), does_something(&0))]
#[allow(dead_code)]
fn id(a: &u8) -> &u8 {
    a
}
