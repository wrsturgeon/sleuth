#![deny(warnings)]
#![warn(clippy::missing_docs_in_private_items)]
#![allow(unreachable_code)]

mod cover;
mod fn_predicate;

use cover::*;
use fn_predicate::*;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn fn_predicate(attr: TokenStream, input: TokenStream) -> TokenStream {
    match fn_predicate_impl(attr.into(), input.into()) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

#[proc_macro_attribute]
pub fn cover(attr: TokenStream, input: TokenStream) -> TokenStream {
    match cover_impl(attr.into(), input.into()) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
