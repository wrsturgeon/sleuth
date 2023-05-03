//! Utilities for consuming a usual function and spitting out an AST-aligned representation we can mutate later.

#![allow(
    dead_code,
    unreachable_code,
    clippy::todo,
    clippy::diverging_sub_expression,
    clippy::panic_in_result_fn
)] // TODO: remove

use crate::{expr_path, ident, path, pathseg, punctuate};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, TypePath};

/// Ask *you* for your contributions!
macro_rules! community_input {
    ($arg:expr) => {
        return Err(syn::Error::new($arg.span(), format!("Sleuth doesn't know how to handle this case yet, but we'd love your help! If you're interested, please look into what went wrong and open a PR.\r\nFailed at {}:{}\r\n{} = {:#?}", file!(), line!(), stringify!($arg), $arg)))
    };
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn ast_fn_expr(fnname: &str) -> Expr {
    Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: ast_fn(fnname),
    })
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn ast_fn(fnname: &str) -> syn::Path {
    path(
        true,
        punctuate([
            pathseg(ident(crate::CRATE_NAME)),
            pathseg(ident("f")),
            pathseg(ident(fnname)),
        ]),
    )
}

/// Call one of our in-house functions mimicking an AST node.
#[inline]
fn call(fnname: &str, args: Punctuated<Expr, syn::token::Comma>) -> Expr {
    Expr::Call(syn::ExprCall {
        attrs: vec![],
        func: Box::new(ast_fn_expr(fnname)),
        paren_token: delim_token!(Paren),
        args,
    })
}

/// Type and initializer, both starting with `::sleuth::expr::`
#[inline]
fn node<const N: usize, const M: usize>(
    what: &str,
    generics: [TypePath; N],
    args: [Expr; M],
) -> Node {
    let mut whole_enchilada =
        punctuate([pathseg(ident(crate::CRATE_NAME)), pathseg(ident("expr"))]);
    whole_enchilada.push(syn::PathSegment {
        ident: ident(what),
        arguments: if N == 0 {
            syn::PathArguments::None
        } else {
            syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                colon2_token: Some(dual_token!(PathSep)),
                lt_token: token!(Lt),
                args: punctuate(
                    generics.map(move |t| syn::GenericArgument::Type(syn::Type::Path(t))),
                ),
                gt_token: token!(Gt),
            })
        },
    });
    (
        syn::TypePath {
            qself: None,
            path: path(true, whole_enchilada.clone()),
        },
        if M == 0 {
            expr_path(
                true,
                whole_enchilada
                    .into_iter()
                    .map(move |p| pathseg(p.ident) /* no turbofish */)
                    .collect(),
            )
        } else {
            Expr::Call(syn::ExprCall {
                attrs: vec![],
                func: Box::new(expr_path(
                    true,
                    whole_enchilada
                        .into_iter()
                        .map(move |p| pathseg(p.ident) /* no turbofish */)
                        .collect(),
                )),
                paren_token: delim_token!(Paren),
                args: punctuate(args),
            })
        },
    )
}
/// A type and its instantiation.
type Node = (TypePath, Expr);
/// Either a type and its instantiation or failure.
type MaybeNode = Result<Node, syn::Error>;

/// Parse a function into an equivalent function that can be mutated.
pub fn function(f: &syn::ItemFn) -> MaybeNode {
    block(f.block.as_ref())
}

/// Parse a block into an equivalent block that can be mutated.
pub fn block(b: &syn::Block) -> MaybeNode {
    let (stmt_type, stmt_init) = statements(&b.stmts)?;
    Ok(node("Block", [stmt_type], [stmt_init]))
}

/// Parse a set of statements.
pub fn statements(stmts: &[syn::Stmt]) -> MaybeNode {
    #![allow(clippy::shadow_unrelated)] // for some reason it can't tell this is assignment (I think?)
    let (last, not_last) = match stmts.split_last() {
        None => return Ok(node("EndList", [], [])),
        Some(splat) => splat,
    };
    let (mut t, mut e) = if let syn::Stmt::Expr(e, None) = last {
        let (r_t, r_e) = expression(e)?; // Last expression with no semicolon => locally returning its value
        node("LastStatement", [r_t], [r_e])
    } else {
        let (s_t, s_e) = statement(last)?;
        let (l_t, l_e) = node("EndList", [], []);
        node("StatementList", [s_t, l_t], [s_e, l_e])
    };
    for s in not_last {
        let (s_t, s_e) = statement(s)?;
        (t, e) = node("StatementList", [s_t, t], [s_e, e]);
    }
    Ok((t, e))
}

/// Parse a statement into an equivalent statement that can be mutated.
pub fn statement(s: &syn::Stmt) -> MaybeNode {
    match s {
        syn::Stmt::Expr(e, None) => expression(e),
        _ => community_input!(s),
    }
}

/// Parse an expression into an equivalent expression that can be mutated.
pub fn expression(e: &Expr) -> MaybeNode {
    match e {
        p @ Expr::Path(_) => Ok(node(
            "Path",
            [],
            [Expr::Verbatim(
                p.to_token_stream().to_string().into_token_stream(),
            )],
        )),
        Expr::Lit(li) => literal(li),
        Expr::Binary(b) => binary_expression(b),
        Expr::If(i) => conditional(i),
        Expr::Block(b) => block(&b.block),
        _ => community_input!(e),
    }
}

/// Parse a conditional (i.e. `if`) into an equivalent call expression that can be mutated.
pub fn conditional(e: &syn::ExprIf) -> MaybeNode {
    let (cond_t, cond_e) = expression(e.cond.as_ref())?;
    let (left_t, left_e) = block(&e.then_branch)?;
    Ok(if let Some((_, right)) = &e.else_branch {
        let (right_t, right_e) = expression(right.as_ref())?;
        node(
            "IfElse",
            [cond_t, left_t, right_t],
            [cond_e, left_e, right_e],
        )
    } else {
        node("If", [cond_t, left_t], [cond_e, left_e])
    })
}

/// Parse a literal (e.g. `0`, `true`, `"hi"`) into an equivalent call expression that can be mutated.
pub fn literal(e: &syn::ExprLit) -> MaybeNode {
    Ok(node(
        "Literal",
        [syn::TypePath {
            qself: None,
            path: path(
                false,
                punctuate([pathseg(ident(match &e.lit {
                    syn::Lit::Bool(_) => "bool",
                    syn::Lit::Int(_) => "isize",
                    _ => community_input!(e.lit),
                }))]),
            ),
        }],
        [Expr::Lit(e.clone())],
    ))
}

/// Parse a binary expression into an equivalent call expression that can be mutated.
pub fn binary_expression(e: &syn::ExprBinary) -> MaybeNode {
    let (left_t, left_e) = expression(e.left.as_ref())?;
    let (right_t, right_e) = expression(e.right.as_ref())?;
    Ok(node(
        match e.op {
            syn::BinOp::Add(_) => "Add",
            syn::BinOp::Sub(_) => "Sub",
            _ => community_input!(e),
        },
        [left_t, right_t],
        [left_e, right_e],
    ))
}
