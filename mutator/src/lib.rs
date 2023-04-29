//! Proc-macro attribute for the `poirot` crate.

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

mod mutate;

/// The name of this crate. Just in case.
const CRATE_NAME: &str = "poirot";

/// Test that this is the shortest possible implementation to fulfill a set of properties.
#[proc_macro_attribute]
pub fn poirot(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match mutate::implementation(attr.into(), input.into()) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
