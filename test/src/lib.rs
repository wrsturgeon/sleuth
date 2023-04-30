//! Testing framework using `sleuth` as an `extern crate`.

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
    clippy::mod_module_files,
    clippy::pattern_type_mismatch,
    clippy::pub_use,
    clippy::question_mark_used,
    clippy::string_add,
    clippy::wildcard_enum_match_arm,
    clippy::wildcard_imports
)]
#![deny(warnings)]

mod sleuth;

use ::sleuth::sleuth;

/// Trivial identity function to test the basics.
#[sleuth(
    does_nothing(0),
    does_nothing(1),
    !does_something(42)
)]
#[allow(dead_code)]
const fn id(a: u8) -> u8 {
    a
}

#[sleuth(
    roundtrip(sub_one, 42),
    !roundtrip(add_one, 42),
)]
#[allow(dead_code)]
const fn add_one(x: u8) -> u8 {
    x + 1
}

#[sleuth(
    roundtrip(add_one, 42),
    !roundtrip(sub_one, 42),
)]
#[allow(dead_code)]
const fn sub_one(x: u8) -> u8 {
    x - 1
}
