//! Utilities for consuming a usual function and spitting out an AST-aligned representation we can mutate later.

#![allow(dead_code, unreachable_code)] // TODO: remove

use crate::{expr_path, ident, path, pathseg, punctuate};
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
    let mut ps = punctuate(pathseg(ident(crate::CRATE_NAME)));
    ps.push(syn::PathSegment {
        ident: ident("f"),
        arguments: syn::PathArguments::None,
    });
    ps.push(syn::PathSegment {
        ident: ident(fnname),
        arguments: syn::PathArguments::None,
    });
    path(true, ps)
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

/// Parse a function into an equivalent function that can be mutated.
pub fn function(f: &syn::ItemFn) -> Result<(syn::Type, syn::Expr), syn::Error> {
    block(f.block.as_ref())
}

/// Parse a block into an equivalent block that can be mutated.
pub fn block(b: &syn::Block) -> Result<(syn::Type, syn::Expr), syn::Error> {
    let (stmt_type, stmt_init) = statements(&b.stmts)?;
    let mut sleuth_expr_init = punctuate(pathseg(ident(crate::CRATE_NAME)));
    sleuth_expr_init.push(pathseg(ident("expr")));
    sleuth_expr_init.push(pathseg(ident("Block")));
    let mut sleuth_expr_type = punctuate(pathseg(ident(crate::CRATE_NAME)));
    sleuth_expr_type.push(pathseg(ident("expr")));
    sleuth_expr_type.push(syn::PathSegment {
        ident: ident("Block"),
        arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: token!(Lt),
            args: punctuate(syn::GenericArgument::Type(stmt_type)),
            gt_token: token!(Gt),
        }),
    });
    Ok((
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: path(true, sleuth_expr_type),
        }),
        syn::Expr::Call(syn::ExprCall {
            attrs: vec![],
            func: Box::new(expr_path(true, sleuth_expr_init)),
            paren_token: delim_token!(Paren),
            args: punctuate(stmt_init),
        }),
    ))
}

pub fn statements(_stmts: &Vec<syn::Stmt>) -> Result<(syn::Type, syn::Expr), syn::Error> {
    let mut end = punctuate(pathseg(ident(crate::CRATE_NAME)));
    end.push(pathseg(ident("expr")));
    end.push(pathseg(ident("EndOfBlock")));
    let rtype = syn::Type::Path(syn::TypePath {
        qself: None,
        path: path(true, end.clone()),
    });
    let rexpr = expr_path(true, end);
    // let mut literal = punctuate(pathseg(ident(crate::CRATE_NAME)));
    // literal.push(pathseg(ident("expr")));
    // literal.push(syn::PathSegment {
    //     ident: ident("Literal"),
    //     arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
    //         colon2_token: None,
    //         lt_token: token!(Lt),
    //         args: punctuate(syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
    //             qself: None,
    //             path: path(false,punctuate(pathseg(ident("bool")))),
    //         }))),
    //         gt_token: token!(Gt),
    //     }),
    // });
    // Ok((
    //     syn::Type::Path(syn::TypePath {
    //         qself: None,
    //         path: path(true, literal),
    //     }),
    //     expr_path(false,punctuate(pathseg(ident("false")))),
    // ))
    Ok((rtype, rexpr))
}

/// Parse a statement into an equivalent statement that can be mutated.
pub fn stmt(s: &syn::Stmt) -> Result<syn::Stmt, syn::Error> {
    match s {
        syn::Stmt::Expr(e, None) => Ok(syn::Stmt::Expr(call("rtn", punctuate(expr(e)?)), None)),
        _ => community_input!(s),
    }
}

/// Parse an expression into an equivalent expression that can be mutated.
pub fn expr(e: &syn::Expr) -> Result<syn::Expr, syn::Error> {
    match e {
        syn::Expr::Path(p) => Ok(call("path", punctuate(syn::Expr::Path(p.clone())))),
        syn::Expr::Lit(li) => Ok(call("literal", punctuate(syn::Expr::Lit(li.clone())))),
        syn::Expr::Binary(b) => expr_binary(b),
        syn::Expr::If(i) => cond(i),
        syn::Expr::Block(b) => Ok(syn::Expr::Block(syn::ExprBlock {
            attrs: b.attrs.clone(),
            label: b.label.clone(),
            block: todo!(), // block(&b.block)?,
        })),
        _ => community_input!(e),
    }
}

/// Parse a binary expression into an equivalent call expression that can be mutated.
pub fn expr_binary(e: &syn::ExprBinary) -> Result<syn::Expr, syn::Error> {
    let mut args = punctuate(expr(e.left.as_ref())?);
    args.push(expr(e.right.as_ref())?);
    match e.op {
        syn::BinOp::Add(_) => Ok(call("add", args)),
        syn::BinOp::Sub(_) => Ok(call("sub", args)),
        _ => community_input!(e),
    }
}

/// Parse a conditional (i.e. `if`) into an equivalent call expression that can be mutated.
pub fn cond(e: &syn::ExprIf) -> Result<syn::Expr, syn::Error> {
    let mut args = punctuate(expr(e.cond.as_ref())?);
    args.push(syn::Expr::Closure(syn::ExprClosure {
        attrs: vec![],
        lifetimes: None,
        constness: None,
        movability: None,
        asyncness: None,
        capture: None,
        or1_token: token!(Or),
        inputs: Punctuated::new(),
        or2_token: token!(Or),
        output: syn::ReturnType::Default,
        body: Box::new(syn::Expr::Block(syn::ExprBlock {
            attrs: vec![],
            label: None,
            block: todo!(), // block(&e.then_branch)?,
        })),
    }));
    if let Some((_, otherwise)) = &e.else_branch {
        args.push(syn::Expr::Closure(syn::ExprClosure {
            attrs: vec![],
            lifetimes: None,
            constness: None,
            movability: None,
            asyncness: None,
            capture: None,
            or1_token: token!(Or),
            inputs: Punctuated::new(),
            or2_token: token!(Or),
            output: syn::ReturnType::Default,
            body: Box::new(expr(otherwise.as_ref())?),
        }));
    }
    Ok(call("cond", args))
}
