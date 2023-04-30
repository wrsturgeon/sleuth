//! Utilities for consuming a usual function and spitting out an AST-aligned representation we can mutate later.

use crate::{ident, make_punc, make_punc_pathseg};
use syn::{punctuated::Punctuated, spanned::Spanned};

macro_rules! community_input {
    ($arg:expr) => {
        return Err(syn::Error::new($arg.span(), format!("Sleuth doesn't know how to handle this case yet, but we'd love your help! If you're interested, please look into what went wrong and open a PR.\r\nFailed at {}:{}\r\n{} = {:#?}", file!(), line!(), stringify!($arg), $arg)))
    };
}

#[inline]
fn ast_fn_expr(fnname: &str) -> syn::Expr {
    syn::Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: ast_fn(fnname),
    })
}

#[inline]
fn ast_fn(fnname: &str) -> syn::Path {
    let mut ps = make_punc_pathseg("sleuth");
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

#[inline]
fn call(fnname: &str, args: Punctuated<syn::Expr, syn::token::Comma>) -> syn::Expr {
    syn::Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(ast_fn_expr(fnname)),
        paren_token: delim_token!(Paren),
        args,
    })
}

pub fn item(ast: &syn::Item) -> Result<syn::Item, syn::Error> {
    match ast {
        syn::Item::Fn(f) => Ok(syn::Item::Fn(syn::ItemFn {
            attrs: f.attrs.clone(),
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
        })),
        _ => community_input!(ast),
    }
}

pub fn block(b: &syn::Block) -> Result<syn::Block, syn::Error> {
    let mut stmts = vec![];
    for s in &b.stmts {
        stmts.push(stmt(s)?);
    }
    Ok(syn::Block {
        brace_token: b.brace_token.clone(),
        stmts,
    })
}

pub fn stmt(s: &syn::Stmt) -> Result<syn::Stmt, syn::Error> {
    match s {
        syn::Stmt::Expr(e, None) => Ok(syn::Stmt::Expr(
            syn::Expr::Return(syn::ExprReturn {
                attrs: vec![],
                return_token: single_token!(Return),
                expr: Some(Box::new(call("rtn", make_punc(expr(e)?)))),
            }),
            Some(token!(Semi)),
        )),
        _ => community_input!(s),
    }
}

pub fn expr(e: &syn::Expr) -> Result<syn::Expr, syn::Error> {
    match e {
        syn::Expr::Path(p) => Ok(call("path", make_punc(syn::Expr::Path(p.clone())))),
        syn::Expr::Lit(li) => Ok(call("literal", make_punc(syn::Expr::Lit(li.clone())))),
        syn::Expr::Binary(b) => expr_binary(b),
        _ => community_input!(e),
    }
}

pub fn expr_binary(e: &syn::ExprBinary) -> Result<syn::Expr, syn::Error> {
    let mut args = make_punc(expr(e.left.as_ref())?);
    args.push(expr(e.right.as_ref())?);
    match e.op {
        syn::BinOp::Add(_) => Ok(call("add", args)),
        syn::BinOp::Sub(_) => Ok(call("sub", args)),
        _ => community_input!(e),
    }
}
