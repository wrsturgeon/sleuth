//! Extremely opinionated testing framework generating an exact specification and reducing code to its minimal implementation.
//! ```rust
//! use ::sleuth::sleuth;
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
//! #[sleuth(
//!     roundtrip(sub_one, 42),
//!     !roundtrip(add_one, 42),
//! )]
//! fn add_one(x: u8) -> u8 {
//!     x + 1
//! }
//!
//! #[sleuth(
//!     roundtrip(add_one, 42),
//!     !roundtrip(sub_one, 42),
//! )]
//! fn sub_one(x: u8) -> u8 {
//!     x - 1
//! }
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
    clippy::exhaustive_structs,
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

pub mod expr;

mod util;

pub use expr::Expr;
pub use sleuth_mutator as mutator;
pub use sleuth_mutator::*;

/// Turns the output of a `timid_assert!` into a test.
/// # Panics
/// When given an argument that is not `None` (with almost exactly the message given).
#[inline]
pub fn testify(check_output: Option<&'static str>) {
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
