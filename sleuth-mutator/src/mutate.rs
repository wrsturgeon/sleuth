//! Behind-the-scenes implementation of the publicly exported macro.

use crate::{ident, pathseg, punctuate};
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item, Stmt};

/// Generic type name for the argument that's automatically filled in when this attribute is applied.
const FN_TYPE_NAME: &str = "_FnToCheck";

/// Name of the argument that's automatically filled in when this attribute is applied.
const FN_ARG_NAME: &str = "f";

/// The `#[cfg(test)]` macro, but able to be turned off for transparency with `cargo expand`.
pub const CFG_MACRO: &str = "cfg"; // set this to "cfg" for building or to nonsense for `cargo expand`

/// The `#[test]` macro, but able to be turned off for transparency with `cargo expand`.
const TEST_MACRO: &str = "test"; // see above, but "test" for actual builds

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

    // Replace the original function definition with an equivalent callable struct that makes the AST explicit
    crate::x::parse::item(&ast)?.to_tokens(&mut ts);

    // Make sure this macro is applied to a function & get the function's AST
    let Item::Fn( f) = ast else { return Err(syn::Error::new(ast.span(), "This macro can be applied only to functions")); };
    if f.sig.unsafety.is_some() {
        return Err(syn::Error::new(
            f.sig.unsafety.span(),
            "Please don't mutate unsafe functions",
        ));
    }

    // Copy the original when we're not testing
    let mut attrs = f.attrs;
    // attrs.push(syn::Attribute {
    //     pound_token: token!(Pound),
    //     style: syn::AttrStyle::Outer,
    //     bracket_token: delim_token!(Bracket),
    //     meta: syn::Meta::List(syn::MetaList {
    //         path: syn::Path {
    //             leading_colon: None,
    //             segments: punctuate(pathseg(ident("allow"))),
    //         },
    //         delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
    //         tokens: ident("dead_code").into_token_stream(),
    //     }),
    // });
    attrs.push(syn::Attribute {
        pound_token: token!(Pound),
        style: syn::AttrStyle::Outer,
        bracket_token: delim_token!(Bracket),
        meta: syn::Meta::List(syn::MetaList {
            path: syn::Path {
                leading_colon: None,
                segments: punctuate(pathseg(ident(CFG_MACRO))),
            },
            delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
            tokens: syn::ExprCall {
                attrs: vec![],
                func: Box::new(Expr::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: punctuate(pathseg(ident("not"))),
                    },
                })),
                paren_token: delim_token!(Paren),
                args: punctuate(Expr::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: punctuate(pathseg(ident("test"))),
                    },
                })),
            }
            .into_token_stream(),
        }),
    });
    Item::Fn(syn::ItemFn {
        attrs,
        vis: f.vis,
        sig: f.sig.clone(),
        block: f.block,
    })
    .to_tokens(&mut ts);

    // Make a #[cfg(test)] module with a checker (that doesn't panic and isn't a test) and a test (that calls the checker)
    make_fn_specific_module(
        ident(&(f.sig.ident.to_string() + "_sleuth")),
        make_checker(
            single_predicate_from_arguments(attr)?,
            make_fn_trait_bound(f.sig.inputs, f.sig.output)?,
        ),
        make_test(f.sig.ident),
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

        let mut punc_args = punctuate(Expr::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: punctuate(pathseg(ident(FN_ARG_NAME))),
            },
        }));
        punc_args.push(Expr::Verbatim(fn_args.stream()));

        let mut fn_path = punctuate(pathseg(ident("crate")));
        fn_path.push(pathseg(ident(crate::CRATE_NAME)));
        fn_path.push(pathseg(fn_name));
        let mut timid_assert = punctuate(pathseg(ident("sleuth")));
        timid_assert.push(pathseg(ident(if should_fail {
            TIMID_ASSERT_FALSE_MACRO
        } else {
            TIMID_ASSERT_MACRO
        })));
        let this_pred = syn::Expr::Macro(syn::ExprMacro {
            attrs: vec![],
            mac: syn::Macro {
                path: syn::Path {
                    leading_colon: Some(dual_token!(PathSep)),
                    segments: timid_assert,
                },
                bang_token: token!(Not),
                delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                tokens: Expr::Call(syn::ExprCall {
                    attrs: vec![],
                    func: Box::new(Expr::Path(syn::ExprPath {
                        attrs: vec![],
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: fn_path,
                        },
                    })),
                    paren_token: delim_token!(Paren),
                    args: punc_args,
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
                args: punctuate(Expr::Closure(syn::ExprClosure {
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
                })),
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

/// Constructs a `Fn` trait to match the signature of the function this attribute was applied to.
#[inline]
fn make_fn_trait_bound(
    parsed_fn_inputs: Punctuated<syn::FnArg, syn::token::Comma>,
    parsed_fn_output: syn::ReturnType,
) -> Result<Punctuated<syn::TypeParamBound, syn::token::Plus>, syn::Error> {
    let mut input_types = Punctuated::new();
    for arg in parsed_fn_inputs {
        match arg {
            #![allow(clippy::match_wildcard_for_single_variants)]
            syn::FnArg::Typed(t) => input_types.push(*t.ty),
            _ => {
                return Err(syn::Error::new(
                    arg.span(),
                    "Something went wrong with function argument types",
                ))
            }
        }
    }
    let mut punc = punctuate(syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: syn::Path {
            leading_colon: None,
            segments: punctuate(syn::PathSegment {
                ident: ident("Fn"),
                arguments: syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    paren_token: delim_token!(Paren),
                    inputs: input_types,
                    output: parsed_fn_output,
                }),
            }),
        },
    }));
    let mut ref_unwind_safe = punctuate(pathseg(ident("core")));
    ref_unwind_safe.push(pathseg(ident("panic")));
    ref_unwind_safe.push(pathseg(ident("RefUnwindSafe")));
    punc.push(syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: syn::Path {
            leading_colon: None,
            segments: ref_unwind_safe,
        },
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
                path: syn::Path {
                    leading_colon: None,
                    segments: punctuate(pathseg(ident("inline"))),
                },
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
                params: punctuate(syn::GenericParam::Type(syn::TypeParam {
                    attrs: vec![],
                    ident: ident(FN_TYPE_NAME),
                    colon_token: Some(token!(Colon)),
                    bounds: fn_trait_bound,
                    eq_token: None,
                    default: None,
                })),
                gt_token: Some(token!(Gt)),
                where_clause: None,
            },
            paren_token: delim_token!(Paren),
            inputs: punctuate(syn::FnArg::Typed(syn::PatType {
                attrs: vec![],
                pat: Box::new(syn::Pat::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: punctuate(pathseg(ident(FN_ARG_NAME))),
                    },
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
                            segments: punctuate(pathseg(ident(FN_TYPE_NAME))),
                        },
                    })),
                })),
            })),
            variadic: None,
            output: syn::ReturnType::Type(
                dual_token!(RArrow),
                Box::new(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: punctuate(syn::PathSegment {
                            ident: ident("Option"),
                            arguments: syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: token!(Lt),
                                    args: punctuate(syn::GenericArgument::Type(
                                        syn::Type::Reference(syn::TypeReference {
                                            and_token: token!(And),
                                            lifetime: None,
                                            mutability: None,
                                            elem: Box::new(syn::Type::Path(syn::TypePath {
                                                qself: None,
                                                path: syn::Path {
                                                    leading_colon: None,
                                                    segments: punctuate(pathseg(ident("str"))),
                                                },
                                            })),
                                        }),
                                    )),
                                    gt_token: token!(Gt),
                                },
                            ),
                        }),
                    },
                })),
            ),
        },
        block: Box::new(syn::Block {
            brace_token: delim_token!(Brace),
            stmts: vec![Stmt::Expr(pred, None)],
        }),
    })
}

/// Builds a test that panics if all the given checks don't hold.
#[inline]
fn make_test(parsed_fn_sig_ident: Ident) -> Item {
    let mut sleuth_testify = punctuate(pathseg(ident(crate::CRATE_NAME)));
    sleuth_testify.push(pathseg(ident("testify")));
    let mut fn_to_test = punctuate(pathseg(ident("super")));
    fn_to_test.push(pathseg(parsed_fn_sig_ident));
    Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::Path(syn::Path {
                leading_colon: None,
                segments: punctuate(pathseg(ident(TEST_MACRO))),
            }),
        }],
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: single_token!(Fn),
            ident: ident("test"),
            generics: syn::Generics {
                lt_token: None,
                params: Punctuated::new(),
                gt_token: None,
                where_clause: None,
            },
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
                    func: Box::new(Expr::Path(syn::ExprPath {
                        attrs: vec![],
                        qself: None,
                        path: syn::Path {
                            leading_colon: Some(dual_token!(PathSep)),
                            segments: sleuth_testify,
                        },
                    })),
                    paren_token: delim_token!(Paren),
                    args: punctuate(Expr::Call(syn::ExprCall {
                        attrs: vec![],
                        func: Box::new(Expr::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: syn::Path {
                                leading_colon: None,
                                segments: punctuate(pathseg(ident("check"))),
                            },
                        })),
                        paren_token: delim_token!(Paren),
                        args: punctuate(Expr::Reference(syn::ExprReference {
                            attrs: vec![],
                            and_token: token!(And),
                            mutability: None,
                            expr: Box::new(Expr::Path(syn::ExprPath {
                                attrs: vec![],
                                qself: None,
                                path: syn::Path {
                                    leading_colon: None,
                                    segments: fn_to_test,
                                },
                            })),
                        })),
                    })),
                }),
                None,
            )],
        }),
    })
}

/// Builds a module with a non-panicking checker and a test using the former to know when to panic.
#[inline]
fn make_fn_specific_module(mod_ident: Ident, check_fn: Item, test_fn: Item) -> Item {
    syn::Item::Mod(syn::ItemMod {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::List(syn::MetaList {
                path: syn::Path {
                    leading_colon: None,
                    segments: punctuate(pathseg(ident(CFG_MACRO))),
                },
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
                check_fn,
                test_fn,
            ],
        )),
        semi: None,
    })
}
