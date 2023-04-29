//! Testing framework using `poirot` as an `extern crate`.

#![warn(
    missing_docs,
    rustdoc::all,
    clippy::missing_docs_in_private_items,
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery
)]
#![allow(
    clippy::blanket_clippy_restriction_lints,
    clippy::implicit_return,
    clippy::integer_arithmetic,
    clippy::pub_use,
    clippy::question_mark_used,
    clippy::string_add,
    clippy::too_many_lines,
    clippy::wildcard_imports
)]
#![deny(warnings)]

mod poirot;

use ::poirot::poirot;

/// Trivial identity function to test the basics.
#[poirot(does_nothing(&0), does_nothing(&1), does_something(&0))]
#[allow(dead_code)]
const fn id(a: &u8) -> &u8 {
    a
}

#[allow(
    dead_code,
    clippy::missing_docs_in_private_items,
    clippy::arithmetic_side_effects
)]
#[poirot(roundtrip(sub_one, 1), roundtrip(sub_one, 42))]
const fn add_one(x: u8) -> u8 {
    x + 1
}

#[allow(
    dead_code,
    clippy::missing_docs_in_private_items,
    clippy::arithmetic_side_effects
)]
#[poirot(roundtrip(add_one, 1), roundtrip(add_one, 42))]
const fn sub_one(x: u8) -> u8 {
    x - 1
}
