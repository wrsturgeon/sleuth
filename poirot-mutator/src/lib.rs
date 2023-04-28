#![deny(warnings)]
#![warn(clippy::missing_docs_in_private_items)]
#![allow(unreachable_code)]

mod cover;

use cover::*;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn poirot(attr: TokenStream, input: TokenStream) -> TokenStream {
    match cover_impl(attr.into(), input.into()) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
