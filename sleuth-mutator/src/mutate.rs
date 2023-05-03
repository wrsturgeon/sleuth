//! Behind-the-scenes implementation of the publicly exported macro.

use crate::{expr_path, ident, path, pathseg, punctuate, NO_GENERICS};
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item, Stmt, Type};

/// Generic type name for the argument that's automatically filled in when this attribute is applied.
const FN_TYPE_NAME: &str = "_FnToCheck";

/// Name of the argument that's automatically filled in when this attribute is applied.
const FN_ARG_NAME: &str = "f";

/// The `#[cfg(test)]` macro, but able to be turned off for transparency with `cargo expand`.
pub const CFG_MACRO: &str = if option_env!("EXPAND").is_some() {
    "cfg_when_not_expand"
} else {
    "cfg"
};

/// The `#[test]` macro, but able to be turned off for transparency with `cargo expand`.
const TEST_MACRO: &str = if option_env!("EXPAND").is_some() {
    "test_when_not_expand"
} else {
    "test"
};

/// Name of the macro that returns a potential error message without `panic`king.
const TIMID_ASSERT_MACRO: &str = "timid_assert";

/// Name of the macro that returns a potential error message ON FALSE without `panic`king.
const TIMID_ASSERT_FALSE_MACRO: &str = "timid_assert_false";

/// Actually parse the AST and turn it into something useful.
#[inline]
pub fn implementation(attr: TokenStream, input: TokenStream) -> Result<TokenStream, syn::Error> {
    if attr.is_empty() {
        return Err(syn::Error::new(
            attr.span(),
            "Covering zero predicates doesn't mean much; add some!",
        ));
    }

    // Accumulator for our output AST
    let mut ts = TokenStream::new();

    // Parse the entire function body
    let ast: Item = syn::parse2(input)?;
    // and paste it again verbatim
    ast.to_tokens(&mut ts);

    // Make sure this macro is applied to a function & get the function's AST
    let Item::Fn( f) = ast else { return Err(syn::Error::new(ast.span(), "This macro can be applied only to functions")); };
    if f.sig.unsafety.is_some() {
        return Err(syn::Error::new(
            f.sig.unsafety.span(),
            "Please don't mutate unsafe functions",
        ));
    }

    // Replace the original function definition with an equivalent callable struct that makes the AST explicit
    let (ast_type, ast_init) = crate::parse::function(&f)?;

    // Make a struct holding all function arguments
    let mut fn_inputs = Punctuated::new();
    for arg in &f.sig.inputs {
        fn_inputs.push(match arg {
            syn::FnArg::Receiver(_) => {
                return Err(syn::Error::new(
                    arg.span(),
                    "Somehow got a method receiver rather than an argument",
                ))
            }
            syn::FnArg::Typed(pt) => match pt.pat.as_ref() {
                syn::Pat::Ident(i) => syn::Field {
                    attrs: vec![],
                    vis: syn::Visibility::Public(single_token!(Pub)),
                    mutability: syn::FieldMutability::None,
                    ident: Some(i.ident.clone()),
                    colon_token: Some(token!(Colon)),
                    ty: pt.ty.as_ref().clone(),
                },
                _ => {
                    return Err(syn::Error::new(
                        pt.span(),
                        "Argument name is somehow not an identifier",
                    ))
                }
            },
        });
    }

    // Make the #[cfg(test)] module holding everything we'll ever use
    make_fn_specific_module(
        ident(&(f.sig.ident.to_string() + "_sleuth")),
        make_scoped_variables(&fn_inputs)?,
        make_scope_struct(fn_inputs),
        ast_type,
        ast_init,
        make_checker(
            single_predicate_from_arguments(attr)?,
            make_fn_trait_bound(f.sig.inputs, f.sig.output)?,
        ),
        make_test_original(f.sig.ident.clone()),
        make_test_mutants(f.sig.ident),
    )
    .to_tokens(&mut ts);

    Ok(ts)
}

/// Fold all arguments into a call that will calmly return the error message (not panic!) for the first check to fail.
#[allow(clippy::too_many_lines)]
#[inline]
fn single_predicate_from_arguments(attr: TokenStream) -> Result<Expr, syn::Error> {
    let mut pred = None;
    let mut preds = syn::parse2::<TokenStream>(attr)?.into_iter();
    'arg_loop: while let Some(maybe_fn_name) = preds.next() {
        let (should_fail, fn_name) = match maybe_fn_name {
            TokenTree::Ident(i) => (false, i),
            TokenTree::Punct(ref p) => {
                if p.as_char() == '!' {
                    if let Some(TokenTree::Ident(i)) = preds.next() {
                        (true, i)
                    } else {
                        return Err(syn::Error::new(
                            maybe_fn_name.span(),
                            "Expected function name",
                        ));
                    }
                } else {
                    return Err(syn::Error::new(
                        maybe_fn_name.span(),
                        "Expected function name",
                    ));
                }
            }
            _ => {
                return Err(syn::Error::new(
                    maybe_fn_name.span(),
                    "Expected function name",
                ))
            }
        };

        let maybe_fn_args = preds.next();
        let Some(TokenTree::Group(fn_args)) = maybe_fn_args else { return Err(syn::Error::new(maybe_fn_args.span(), "Expected function arguments")); };

        let this_pred = syn::Expr::Macro(syn::ExprMacro {
            attrs: vec![],
            mac: syn::Macro {
                path: path(
                    true,
                    punctuate([
                        pathseg(ident(crate::CRATE_NAME)),
                        pathseg(ident(if should_fail {
                            TIMID_ASSERT_FALSE_MACRO
                        } else {
                            TIMID_ASSERT_MACRO
                        })),
                    ]),
                ),
                bang_token: token!(Not),
                delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                tokens: Expr::Call(syn::ExprCall {
                    attrs: vec![],
                    func: Box::new(expr_path(
                        false,
                        punctuate([
                            pathseg(ident("crate")),
                            pathseg(ident(crate::CRATE_NAME)),
                            pathseg(fn_name),
                        ]),
                    )),
                    paren_token: delim_token!(Paren),
                    args: punctuate([
                        expr_path(false, punctuate([pathseg(ident(FN_ARG_NAME))])),
                        Expr::Verbatim(fn_args.stream()),
                    ]),
                })
                .into_token_stream(),
            },
        });

        pred = Some(if let Some(some_pred) = pred {
            Expr::MethodCall(syn::ExprMethodCall {
                attrs: vec![],
                receiver: Box::new(some_pred),
                dot_token: token!(Dot),
                method: ident("or_else"),
                turbofish: None,
                paren_token: delim_token!(Paren),
                args: punctuate([Expr::Closure(syn::ExprClosure {
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
                    body: Box::new(this_pred),
                })]),
            })
        } else {
            this_pred
        });

        let maybe_maybe_comma = preds.next();
        if let Some(maybe_comma) = maybe_maybe_comma {
            let TokenTree::Punct(ref comma) = maybe_comma else { return Err(syn::Error::new(maybe_comma.span(), "Expected a comma")); };
            if comma.as_char() != ',' {
                return Err(syn::Error::new(maybe_comma.span(), "Expected a comma"));
            }
        } else {
            break 'arg_loop;
        }
    }

    pred.ok_or_else(|| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "No predicates supplied to the macro attribute",
        )
    })
}

/// Make a module containing AST nodes for each local variable.
#[inline]
fn make_scoped_variables(
    fn_inputs: &Punctuated<syn::Field, syn::Token![,]>,
) -> Result<Item, syn::Error> {
    Ok(Item::Mod(syn::ItemMod {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        unsafety: None,
        mod_token: single_token!(Mod),
        ident: ident("scoped_variables"),
        content: Some((
            delim_token!(Brace),
            fn_inputs
                .into_iter()
                .map(|f| {
                    use heck::ToUpperCamelCase;
                    Item::Struct(syn::ItemStruct {
                        attrs: vec![],
                        vis: syn::Visibility::Public(single_token!(Pub)),
                        struct_token: single_token!(Struct),
                        ident: f.ident.as_ref().map_or_else(
                            || ident("_UNNAMED"),
                            |i| ident(i.to_string().to_upper_camel_case().as_str()),
                        ),
                        generics: NO_GENERICS,
                        fields: syn::Fields::Unit,
                        semi_token: Some(token!(Semi)),
                    })
                })
                .chain(fn_inputs.into_iter().map(|f| {
                    use heck::ToUpperCamelCase;
                    Item::Impl(syn::ItemImpl {
                        attrs: vec![],
                        defaultness: None,
                        unsafety: None,
                        impl_token: single_token!(Impl),
                        generics: NO_GENERICS,
                        trait_: Some((
                            None,
                            path(
                                true,
                                punctuate([pathseg(ident("sleuth")), pathseg(ident("Expr"))]),
                            ),
                            single_token!(For),
                        )),
                        self_ty: Box::new(Type::Path(syn::TypePath {
                            qself: None,
                            path: path(
                                false,
                                punctuate([pathseg(f.ident.as_ref().map_or_else(
                                    || ident("_UNNAMED"),
                                    |i| ident(i.to_string().to_upper_camel_case().as_str()),
                                ))]),
                            ),
                        })),
                        brace_token: delim_token!(Brace),
                        items: vec![
                            syn::ImplItem::Type(syn::ImplItemType {
                                attrs: vec![],
                                vis: syn::Visibility::Inherited,
                                defaultness: None,
                                type_token: single_token!(Type),
                                ident: ident("Return"),
                                generics: NO_GENERICS,
                                eq_token: token!(Eq),
                                ty: f.ty.clone(),
                                semi_token: token!(Semi),
                            }),
                            syn::ImplItem::Type(syn::ImplItemType {
                                attrs: vec![],
                                vis: syn::Visibility::Inherited,
                                defaultness: None,
                                type_token: single_token!(Type),
                                ident: ident("Scope"),
                                generics: NO_GENERICS,
                                eq_token: token!(Eq),
                                ty: Type::Path(syn::TypePath {
                                    qself: None,
                                    path: path(
                                        false,
                                        punctuate([
                                            pathseg(ident("super")),
                                            pathseg(ident("Scope")),
                                        ]),
                                    ),
                                }),
                                semi_token: token!(Semi),
                            }),
                            syn::ImplItem::Const(syn::ImplItemConst {
                                attrs: vec![],
                                vis: syn::Visibility::Inherited,
                                defaultness: None,
                                const_token: single_token!(Const),
                                ident: ident("COMPLEXITY"),
                                generics: NO_GENERICS,
                                colon_token: token!(Colon),
                                ty: Type::Path(syn::TypePath {
                                    qself: None,
                                    path: path(false, punctuate([pathseg(ident("usize"))])),
                                }),
                                eq_token: token!(Eq),
                                expr: Expr::Verbatim(1usize.into_token_stream()),
                                semi_token: token!(Semi),
                            }),
                            syn::ImplItem::Fn(syn::ImplItemFn {
                                attrs: vec![syn::Attribute {
                                    pound_token: token!(Pound),
                                    style: syn::AttrStyle::Outer,
                                    bracket_token: delim_token!(Bracket),
                                    meta: syn::Meta::List(syn::MetaList {
                                        path: path(false, punctuate([pathseg(ident("inline"))])),
                                        delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                                        tokens: ident("always").to_token_stream(),
                                    }),
                                }],
                                vis: syn::Visibility::Inherited,
                                defaultness: None,
                                sig: syn::Signature {
                                    constness: None,
                                    asyncness: None,
                                    unsafety: None,
                                    abi: None,
                                    fn_token: single_token!(Fn),
                                    ident: ident("eval"),
                                    generics: NO_GENERICS,
                                    paren_token: delim_token!(Paren),
                                    inputs: punctuate([
                                        syn::FnArg::Receiver(syn::Receiver {
                                            attrs: vec![],
                                            reference: Some((token!(And), None)),
                                            mutability: None,
                                            self_token: single_token!(SelfValue),
                                            colon_token: None,
                                            ty: Box::new(Type::Reference(syn::TypeReference {
                                                and_token: token!(And),
                                                lifetime: None,
                                                mutability: None,
                                                elem: Box::new(Type::Path(syn::TypePath {
                                                    qself: None,
                                                    path: path(
                                                        false,
                                                        punctuate([pathseg(ident("Self"))]),
                                                    ),
                                                })),
                                            })),
                                        }),
                                        syn::FnArg::Typed(syn::PatType {
                                            attrs: vec![],
                                            pat: Box::new(syn::Pat::Path(syn::ExprPath {
                                                attrs: vec![],
                                                qself: None,
                                                path: path(
                                                    false,
                                                    punctuate([pathseg(ident("scope"))]),
                                                ),
                                            })),
                                            colon_token: token!(Colon),
                                            ty: Box::new(Type::Reference(syn::TypeReference {
                                                and_token: token!(And),
                                                lifetime: None,
                                                mutability: Some(single_token!(Mut)),
                                                elem: Box::new(Type::Path(syn::TypePath {
                                                    qself: None,
                                                    path: path(
                                                        false,
                                                        punctuate([
                                                            pathseg(ident("Self")),
                                                            pathseg(ident("Scope")),
                                                        ]),
                                                    ),
                                                })),
                                            })),
                                        }),
                                    ]),
                                    variadic: None,
                                    output: syn::ReturnType::Type(
                                        dual_token!(RArrow),
                                        Box::new(Type::Path(syn::TypePath {
                                            qself: None,
                                            path: path(
                                                false,
                                                punctuate([
                                                    pathseg(ident("Self")),
                                                    pathseg(ident("Return")),
                                                ]),
                                            ),
                                        })),
                                    ),
                                },
                                block: syn::Block {
                                    brace_token: delim_token!(Brace),
                                    stmts: vec![syn::Stmt::Expr(
                                        Expr::Field(syn::ExprField {
                                            attrs: vec![],
                                            base: Box::new(expr_path(
                                                false,
                                                punctuate([pathseg(ident("scope"))]),
                                            )),
                                            dot_token: token!(Dot),
                                            member: syn::Member::Named(
                                                f.ident.as_ref().map_or_else(
                                                    || ident("_UNNAMED"),
                                                    |i| i.clone(),
                                                ),
                                            ),
                                        }),
                                        None,
                                    )],
                                },
                            }),
                        ],
                    })
                }))
                .collect(),
        )),
        semi: None,
    }))
}

/// Make a struct representing each function arguments. **`let` bindings TODO.**
#[inline]
fn make_scope_struct(fn_inputs: Punctuated<syn::Field, syn::Token![,]>) -> Item {
    Item::Struct(syn::ItemStruct {
        attrs: vec![],
        vis: syn::Visibility::Public(single_token!(Pub)),
        struct_token: single_token!(Struct),
        ident: ident("Scope"),
        generics: NO_GENERICS,
        fields: syn::Fields::Named(syn::FieldsNamed {
            brace_token: delim_token!(Brace),
            named: fn_inputs,
        }),
        semi_token: None,
    })
}

/// Constructs a `Fn` trait to match the signature of the function this attribute was applied to.
#[inline]
fn make_fn_trait_bound(
    parsed_fn_inputs: Punctuated<syn::FnArg, syn::token::Comma>,
    parsed_fn_output: syn::ReturnType,
) -> Result<Punctuated<syn::TypeParamBound, syn::token::Plus>, syn::Error> {
    let mut input_types = Punctuated::new();
    for arg in parsed_fn_inputs {
        if let syn::FnArg::Typed(t) = arg {
            input_types.push(*t.ty);
        } else {
            return Err(syn::Error::new(
                arg.span(),
                "Something went wrong with function argument types",
            ));
        }
    }
    let mut punc = punctuate([syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: path(
            false,
            punctuate([syn::PathSegment {
                ident: ident("Fn"),
                arguments: syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    paren_token: delim_token!(Paren),
                    inputs: input_types,
                    output: parsed_fn_output,
                }),
            }]),
        ),
    })]);
    punc.push(syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: path(
            true,
            punctuate([
                pathseg(ident("core")),
                pathseg(ident("panic")),
                pathseg(ident("RefUnwindSafe")),
            ]),
        ),
    }));
    Ok(punc)
}

/// Builds a function that checks, without panicking, whether all the given checks hold.
#[inline]
fn make_checker(
    pred: Expr,
    fn_trait_bound: Punctuated<syn::TypeParamBound, syn::token::Plus>,
) -> Item {
    syn::Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::List(syn::MetaList {
                path: path(false, punctuate([pathseg(ident("inline"))])),
                delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                tokens: ident("always").into_token_stream(),
            }),
        }],
        vis: syn::Visibility::Public(single_token!(Pub)),
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: single_token!(Fn),
            ident: ident("check"),
            generics: syn::Generics {
                lt_token: Some(token!(Lt)),
                params: punctuate([syn::GenericParam::Type(syn::TypeParam {
                    attrs: vec![],
                    ident: ident(FN_TYPE_NAME),
                    colon_token: Some(token!(Colon)),
                    bounds: fn_trait_bound,
                    eq_token: None,
                    default: None,
                })]),
                gt_token: Some(token!(Gt)),
                where_clause: None,
            },
            paren_token: delim_token!(Paren),
            inputs: punctuate([syn::FnArg::Typed(syn::PatType {
                attrs: vec![],
                pat: Box::new(syn::Pat::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: path(false, punctuate([pathseg(ident(FN_ARG_NAME))])),
                })),
                colon_token: token!(Colon),
                ty: Box::new(syn::Type::Reference(syn::TypeReference {
                    and_token: token!(And),
                    lifetime: None,
                    mutability: None,
                    elem: Box::new(syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: punctuate([pathseg(ident(FN_TYPE_NAME))]),
                        },
                    })),
                })),
            })]),
            variadic: None,
            output: syn::ReturnType::Type(
                dual_token!(RArrow),
                Box::new(Type::Path(syn::TypePath {
                    qself: None,
                    path: path(
                        false,
                        punctuate([syn::PathSegment {
                            ident: ident("Option"),
                            arguments: syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: token!(Lt),
                                    args: punctuate([syn::GenericArgument::Type(Type::Reference(
                                        syn::TypeReference {
                                            and_token: token!(And),
                                            lifetime: Some(syn::Lifetime {
                                                apostrophe: proc_macro2::Span::call_site(),
                                                ident: ident("static"),
                                            }),
                                            mutability: None,
                                            elem: Box::new(Type::Path(syn::TypePath {
                                                qself: None,
                                                path: path(
                                                    false,
                                                    punctuate([pathseg(ident("str"))]),
                                                ),
                                            })),
                                        },
                                    ))]),
                                    gt_token: token!(Gt),
                                },
                            ),
                        }]),
                    ),
                })),
            ),
        },
        block: Box::new(syn::Block {
            brace_token: delim_token!(Brace),
            stmts: vec![Stmt::Expr(pred, None)],
        }),
    })
}

/// Builds a test that panics if all the given checks don't hold for the original function.
#[inline]
fn make_test_original(parsed_fn_sig_ident: Ident) -> Item {
    Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::Path(path(false, punctuate([pathseg(ident(TEST_MACRO))]))),
        }],
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: single_token!(Fn),
            ident: ident("test_original"),
            generics: NO_GENERICS,
            paren_token: delim_token!(Paren),
            inputs: Punctuated::new(),
            variadic: None,
            output: syn::ReturnType::Default,
        },
        block: Box::new(syn::Block {
            brace_token: delim_token!(Brace),
            stmts: vec![Stmt::Expr(
                Expr::Call(syn::ExprCall {
                    attrs: vec![],
                    func: Box::new(expr_path(
                        true,
                        punctuate([pathseg(ident(crate::CRATE_NAME)), pathseg(ident("testify"))]),
                    )),
                    paren_token: delim_token!(Paren),
                    args: punctuate([Expr::Call(syn::ExprCall {
                        attrs: vec![],
                        func: Box::new(expr_path(false, punctuate([pathseg(ident("check"))]))),
                        paren_token: delim_token!(Paren),
                        args: punctuate([Expr::Reference(syn::ExprReference {
                            attrs: vec![],
                            and_token: token!(And),
                            mutability: None,
                            expr: Box::new(expr_path(
                                false,
                                punctuate([pathseg(parsed_fn_sig_ident)]),
                            )),
                        })]),
                    })]),
                }),
                None,
            )],
        }),
    })
}

/// Builds a test that finds and returns a mutant that passes all checks if one exists.
#[inline]
#[allow(clippy::too_many_lines)] // not much we can do to shorten it without making this more confusing
fn make_test_mutants(parsed_fn_sig_ident: Ident) -> Item {
    Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::Path(path(false, punctuate([pathseg(ident(TEST_MACRO))]))),
        }],
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: single_token!(Fn),
            ident: ident("test_mutants"),
            generics: NO_GENERICS,
            paren_token: delim_token!(Paren),
            inputs: Punctuated::new(),
            variadic: None,
            output: syn::ReturnType::Default,
        },
        block: Box::new(syn::Block {
            brace_token: delim_token!(Brace),
            stmts: vec![
                Stmt::Item(Item::Use(syn::ItemUse {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    use_token: single_token!(Use),
                    leading_colon: Some(dual_token!(PathSep)),
                    tree: syn::UseTree::Path(syn::UsePath {
                        ident: ident("sleuth"),
                        colon2_token: dual_token!(PathSep),
                        tree: Box::new(syn::UseTree::Name(syn::UseName {
                            ident: ident("Expr"),
                        })),
                    }),
                    semi_token: token!(Semi),
                })),
                Stmt::Expr(
                    Expr::ForLoop(syn::ExprForLoop {
                        attrs: vec![],
                        label: None,
                        for_token: single_token!(For),
                        pat: Box::new(syn::Pat::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: path(false, punctuate([pathseg(ident("mutation_severity"))])),
                        })),
                        in_token: single_token!(In),
                        expr: Box::new(Expr::Range(syn::ExprRange {
                            attrs: vec![],
                            start: Some(Box::new(Expr::Verbatim(0_usize.to_token_stream()))),
                            limits: syn::RangeLimits::Closed(syn::token::DotDotEq {
                                spans: [
                                    proc_macro2::Span::call_site(),
                                    proc_macro2::Span::call_site(),
                                    proc_macro2::Span::call_site(),
                                ],
                            }),
                            end: Some(Box::new(expr_path(
                                false,
                                punctuate([pathseg(ident("Ast")), pathseg(ident("COMPLEXITY"))]),
                            ))),
                        })),
                        body: syn::Block {
                            brace_token: delim_token!(Brace),
                            stmts: vec![Stmt::Expr(
                                Expr::Call(syn::ExprCall {
                                    attrs: vec![],
                                    func: Box::new(expr_path(
                                        true,
                                        punctuate([
                                            pathseg(ident(crate::CRATE_NAME)),
                                            pathseg(ident("testify")),
                                        ]),
                                    )),
                                    paren_token: delim_token!(Paren),
                                    args: punctuate([Expr::Call(syn::ExprCall {
                                        attrs: vec![],
                                        func: Box::new(expr_path(
                                            false,
                                            punctuate([pathseg(ident("check"))]),
                                        )),
                                        paren_token: delim_token!(Paren),
                                        args: punctuate([Expr::Reference(syn::ExprReference {
                                            attrs: vec![],
                                            and_token: token!(And),
                                            mutability: None,
                                            expr: Box::new(expr_path(
                                                false,
                                                punctuate([pathseg(parsed_fn_sig_ident)]),
                                            )),
                                        })]),
                                    })]),
                                }),
                                None,
                            )],
                        },
                    }),
                    None,
                ),
            ],
        }),
    })
}

/// Builds a module with a non-panicking checker and a test using the former to know when to panic.
#[inline]
fn make_fn_specific_module(
    mod_ident: Ident,
    scoped_vars: Item,
    scope_struct: Item,
    ast_type: syn::TypePath,
    ast_init: Expr,
    check_fn: Item,
    test_original: Item,
    test_mutants: Item,
) -> Item {
    syn::Item::Mod(syn::ItemMod {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::List(syn::MetaList {
                path: path(false, punctuate([pathseg(ident(CFG_MACRO))])),
                delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                tokens: ident("test").into_token_stream(),
            }),
        }],
        vis: syn::Visibility::Inherited,
        unsafety: None,
        mod_token: single_token!(Mod),
        ident: mod_ident,
        content: Some((
            delim_token!(Brace),
            vec![
                Item::Use(syn::ItemUse {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    use_token: single_token!(Use),
                    leading_colon: None,
                    tree: syn::UseTree::Path(syn::UsePath {
                        ident: ident("super"),
                        colon2_token: dual_token!(PathSep),
                        tree: Box::new(syn::UseTree::Glob(syn::UseGlob {
                            star_token: token!(Star),
                        })),
                    }),
                    semi_token: token!(Semi),
                }),
                scoped_vars,
                scope_struct,
                syn::Item::Type(syn::ItemType {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    type_token: single_token!(Type),
                    ident: ident("Ast"),
                    generics: NO_GENERICS,
                    eq_token: token!(Eq),
                    ty: Box::new(syn::Type::Path(ast_type)),
                    semi_token: token!(Semi),
                }),
                syn::Item::Const(syn::ItemConst {
                    attrs: vec![],
                    vis: syn::Visibility::Inherited,
                    const_token: single_token!(Const),
                    ident: ident("AST"),
                    generics: NO_GENERICS,
                    colon_token: token!(Colon),
                    ty: Box::new(Type::Path(syn::TypePath {
                        qself: None,
                        path: path(false, punctuate([pathseg(ident("Ast"))])),
                    })),
                    eq_token: token!(Eq),
                    expr: Box::new(ast_init),
                    semi_token: token!(Semi),
                }),
                check_fn,
                test_original,
                test_mutants,
            ],
        )),
        semi: None,
    })
}
