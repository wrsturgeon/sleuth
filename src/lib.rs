//! Extremely opinionated testing framework generating an exact specification and reducing code to its minimal implementation.
//! ```rust
//! use ::poirot::poirot;
//!
//! fn roundtrip<T, U, F, G>(f: F, g: G, x: T) -> bool
//!   where
//!   T: PartialEq + Clone,
//!   F: Fn(U) -> T,
//!   G: Fn(T) -> U,
//! {
//!   x.clone() == f(g(x))
//! }
//!
//! #[poirot(roundtrip(sub_one, 1), roundtrip(sub_one, 42))]
//! fn add_one(x: u8) -> u8 { x + 1 }
//!
//! #[poirot(roundtrip(add_one, 1), roundtrip(add_one, 42))]
//! fn sub_one(x: u8) -> u8 { x - 1 }
//! ```

// TODO: #![no_std]

#![warn(
    missing_docs,
    rustdoc::all,
    clippy::missing_docs_in_private_items,
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
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

mod util;

pub use mutator::*;

/// Turns the output of a `timid_assert!` into a test.
/// # Panics
/// When given an argument that is not `None` (with almost exactly the message given).
#[inline]
pub fn testify(check_output: Option<&str>) {
    #![allow(clippy::panic)]
    use colored::Colorize;
    let _: Option<()> = check_output.and_then(|e| {
        panic!(
            "{}",
            e.replace(concat!("crate::", env!("CARGO_PKG_NAME"), "::"), "")
                .as_str()
                .on_red()
        )
    });
}