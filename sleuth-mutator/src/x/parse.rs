//! Utilities for consuming a usual function and spitting out an AST-aligned representation we can mutate later.

use crate::{ident, pathseg, punctuate};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned};

/// Ask *you* for your contributions!
macro_rules! community_input {
    ($arg:expr) => {
        return Err(syn::Error::new($arg.span(), format!("Sleuth doesn't know how to handle this case yet, but we'd love your help! If you're interested, please look into what went wrong and open a PR.\r\nFailed at {}:{}\r\n{} = {:#?}", file!(), line!(), stringify!($arg), $arg)))
    };
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn ast_fn_expr(fnname: &str) -> syn::Expr {
    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: ast_fn(fnname),
    })
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn ast_fn(fnname: &str) -> syn::Path {
    let mut ps = punctuate(pathseg(ident("sleuth")));
    ps.push(syn::PathSegment {
        ident: ident("f"),
        arguments: syn::PathArguments::None,
    });
    ps.push(syn::PathSegment {
        ident: ident(fnname),
        arguments: syn::PathArguments::None,
    });
    syn::Path {
        leading_colon: Some(dual_token!(PathSep)),
        segments: ps,
    }
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn call(fnname: &str, args: Punctuated<syn::Expr, syn::token::Comma>) -> syn::Expr {
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(ast_fn_expr(fnname)),
        paren_token: delim_token!(Paren),
        args,
    })
}

/// Parse an item and return an equivalent item that can be mutated.
pub fn item(ast: &syn::Item) -> Result<syn::Item, syn::Error> {
    match ast {
        syn::Item::Fn(f) => Ok(syn::Item::Fn(function(f)?)),
        _ => community_input!(ast),
    }
}

/// Parse a function and return an equivalent function that can be mutated.
pub fn function(f: &syn::ItemFn) -> Result<syn::ItemFn, syn::Error> {
    let mut attrs = f.attrs.clone();
    attrs.push(syn::Attribute {
        pound_token: token!(Pound),
        style: syn::AttrStyle::Outer,
        bracket_token: delim_token!(Bracket),
        meta: syn::Meta::List(syn::MetaList {
            path: syn::Path {
                leading_colon: None,
                segments: punctuate(pathseg(ident(crate::mutate::CFG_MACRO))),
            },
            delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
            tokens: ident("test").into_token_stream(),
        }),
    });
    let mut clippy_const_path = punctuate(pathseg(ident("clippy")));
    clippy_const_path.push(syn::PathSegment {
        ident: ident("missing_const_for_fn"),
        arguments: syn::PathArguments::None,
    });
    attrs.push(syn::Attribute {
        pound_token: token!(Pound),
        style: syn::AttrStyle::Outer,
        bracket_token: delim_token!(Bracket),
        meta: syn::Meta::List(syn::MetaList {
            path: syn::Path {
                leading_colon: None,
                segments: punctuate(pathseg(ident("allow"))),
            },
            delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
            tokens: syn::Path {
                leading_colon: None,
                segments: clippy_const_path,
            }
            .into_token_stream(),
        }),
    });
    Ok(syn::ItemFn {
        attrs,
        vis: f.vis.clone(),
        sig: syn::Signature {
            constness: None,
            asyncness: f.sig.asyncness,
            unsafety: f.sig.unsafety,
            abi: f.sig.abi.clone(),
            fn_token: f.sig.fn_token,
            ident: f.sig.ident.clone(),
            generics: f.sig.generics.clone(),
            paren_token: f.sig.paren_token,
            inputs: f.sig.inputs.clone(),
            variadic: f.sig.variadic.clone(),
            output: f.sig.output.clone(),
        },
        block: Box::new(block(f.block.as_ref())?),
    })
}

/// Parse a block and return an equivalent block that can be mutated.
pub fn block(b: &syn::Block) -> Result<syn::Block, syn::Error> {
    let mut stmts = vec![];
    for s in &b.stmts {
        stmts.push(stmt(s)?);
    }
    Ok(syn::Block {
        brace_token: b.brace_token,
        stmts,
    })
}

/// Parse a statement and return an equivalent statement that can be mutated.
pub fn stmt(s: &syn::Stmt) -> Result<syn::Stmt, syn::Error> {
    match s {
        syn::Stmt::Expr(e, None) => Ok(syn::Stmt::Expr(
            syn::Expr::Return(syn::ExprReturn {
                attrs: vec![],
                return_token: single_token!(Return),
                expr: Some(Box::new(call("rtn", punctuate(expr(e)?)))),
            }),
            Some(token!(Semi)),
        )),
        _ => community_input!(s),
    }
}

/// Parse an expression and return an equivalent expression that can be mutated.
pub fn expr(e: &syn::Expr) -> Result<syn::Expr, syn::Error> {
    match e {
        syn::Expr::Path(p) => Ok(call("path", punctuate(syn::Expr::Path(p.clone())))),
        syn::Expr::Lit(li) => Ok(call("literal", punctuate(syn::Expr::Lit(li.clone())))),
        syn::Expr::Binary(b) => expr_binary(b),
        _ => community_input!(e),
    }
}

/// Parse a binary expression and return an equivalent call expression that can be mutated.
pub fn expr_binary(e: &syn::ExprBinary) -> Result<syn::Expr, syn::Error> {
    let mut args = punctuate(expr(e.left.as_ref())?);
    args.push(expr(e.right.as_ref())?);
    match e.op {
        syn::BinOp::Add(_) => Ok(call("add", args)),
        syn::BinOp::Sub(_) => Ok(call("sub", args)),
        _ => community_input!(e),
    }
}
