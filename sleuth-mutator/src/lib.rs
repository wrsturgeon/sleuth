//! Proc-macro attribute for the `sleuth` crate.

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

/// Fake span for delimiting tokens.
macro_rules! bs_delim_span {
    ($d:ident) => {
        proc_macro2::Group::new(proc_macro2::Delimiter::$d, proc_macro2::TokenStream::new())
            .delim_span()
    };
}

/// Tokens that have a vector of one span instead of one span for some reason, with a fake span for convenience.
macro_rules! token {
    ($t:ident) => {
        syn::token::$t {
            spans: [proc_macro2::Span::call_site()],
        }
    };
}

/// Tokens that have one span instead of a vector of one span for some reason, with a fake span for convenience.
macro_rules! single_token {
    ($t:ident) => {
        syn::token::$t {
            span: proc_macro2::Span::call_site(),
        }
    };
}

/// Tokens that delimit a group, with a fake span for convenience.
macro_rules! dual_token {
    ($t:ident) => {
        syn::token::$t {
            spans: [
                proc_macro2::Span::call_site(),
                proc_macro2::Span::call_site(),
            ],
        }
    };
}

/// Tokens that delimit a group, with a fake span for convenience.
macro_rules! delim_token {
    (Paren) => {
        syn::token::Paren {
            span: bs_delim_span!(Parenthesis),
        }
    };
    ($d:ident) => {
        syn::token::$d {
            span: bs_delim_span!($d),
        }
    };
}

mod mutate;
mod parse;

/// The name of this crate. Just in case.
const CRATE_NAME: &str = "sleuth";

/// Test that this is the shortest possible implementation to fulfill a set of properties.
#[proc_macro_attribute]
pub fn sleuth(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match mutate::implementation(attr.into(), input.into()) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
    .into()
}

/// Make a trivial expression holding a path with or without a leading separator (`::`).
#[inline]
fn expr_path(
    leading: bool,
    punc: syn::punctuated::Punctuated<syn::PathSegment, syn::token::PathSep>,
) -> syn::Expr {
    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: path(leading, punc),
    })
}

/// Make a trivial path with or without a leading separator (`::`).
#[inline]
fn path(
    leading: bool,
    punc: syn::punctuated::Punctuated<syn::PathSegment, syn::token::PathSep>,
) -> syn::Path {
    syn::Path {
        leading_colon: leading.then(|| dual_token!(PathSep)),
        segments: punc,
    }
}

/// Make a trivial punctuated list containing only the argument provided.
#[inline]
fn punctuate<T, P>(v: T) -> syn::punctuated::Punctuated<T, P> {
    let mut punc = syn::punctuated::Punctuated::new();
    punc.push_value(v);
    punc
}

/// Make a trivial identifier from a string.
#[inline]
fn ident(s: &str) -> syn::Ident {
    syn::Ident::new(s, proc_macro2::Span::call_site())
}

/// Make a path segment with only one argument (not really a path, but a singleton, sure).
#[inline]
const fn pathseg(fn_name: syn::Ident) -> syn::PathSegment {
    syn::PathSegment {
        ident: fn_name,
        arguments: syn::PathArguments::None,
    }
}
