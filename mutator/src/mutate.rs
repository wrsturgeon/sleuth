//! Behind-the-scenes implementation of the publicly exported macro.

use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item, Stmt};

/// Generic type name for the argument that's automatically filled in when this attribute is applied.
const FN_TYPE_NAME: &str = "_FnToCheck";

/// Name of the argument that's automatically filled in when this attribute is applied.
const FN_ARG_NAME: &str = "f";

/// The `#[cfg(test)]` macro, but able to be turned off for transparency with `cargo expand`.
const CFG_MACRO: &str = "cfg"; // set this to "cfg" for building or to nonsense for `cargo expand`

/// The `#[test]` macro, but able to be turned off for transparency with `cargo expand`.
const TEST_MACRO: &str = "test"; // see above, but "test" for actual builds

/// Name of the macro that returns a potential error message without `panic`king.
const TIMID_ASSERT_MACRO: &str = "timid_assert";

/// Make a trivial punctuated list containing only the argument provided.
#[inline]
fn make_punc<T, P>(v: T) -> Punctuated<T, P> {
    let mut punc = Punctuated::new();
    punc.push_value(v);
    punc
}

/// Make a trivial identifier from a string.
#[inline]
fn ident(s: &str) -> Ident {
    Ident::new(s, proc_macro2::Span::call_site())
}

/// Fake span for delimiting tokens.
macro_rules! bs_delim_span {
    ($d:ident) => {
        proc_macro2::Group::new(proc_macro2::Delimiter::$d, TokenStream::new()).delim_span()
    };
}

/// Make a punctuated path segment with only one identifier.
#[inline]
fn make_punc_pathseg<P>(s: &str) -> Punctuated<syn::PathSegment, P> {
    make_punc(syn::PathSegment {
        ident: ident(s),
        arguments: syn::PathArguments::None,
    })
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

    // Parse raw input and paste it again verbatim
    let ast: Item = syn::parse2(input)?;
    ast.to_tokens(&mut ts);

    // Make sure this macro is applied to a function & get the function's AST
    let Item::Fn(f) = ast else { return Err(syn::Error::new(ast.span(), "This macro can be applied only to functions")); };
    if f.sig.unsafety.is_some() {
        return Err(syn::Error::new(
            f.sig.unsafety.span(),
            "Please don't mutate unsafe functions",
        ));
    }

    let mut pred = None;
    let mut preds = syn::parse2::<TokenStream>(attr)?.into_iter();
    'arg_loop: while let Some(maybe_fn_name) = preds.next() {
        let TokenTree::Ident(fn_name) = maybe_fn_name else { return Err(syn::Error::new(maybe_fn_name.span(), "Expected function name")); };

        let maybe_fn_args = preds.next();
        let Some(TokenTree::Group(fn_args)) = maybe_fn_args else { return Err(syn::Error::new(maybe_fn_args.span(), "Expected function arguments")); };

        let mut punc_args = make_punc(Expr::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: make_punc_pathseg(FN_ARG_NAME),
            },
        }));
        punc_args.push(Expr::Verbatim(fn_args.stream()));

        let mut fn_path = make_punc_pathseg("crate");
        fn_path.push(syn::PathSegment {
            ident: ident(crate::CRATE_NAME),
            arguments: syn::PathArguments::None,
        });
        fn_path.push(syn::PathSegment {
            ident: fn_name,
            arguments: syn::PathArguments::None,
        });
        let mut timid_assert = make_punc_pathseg("poirot");
        timid_assert.push(syn::PathSegment {
            ident: ident(TIMID_ASSERT_MACRO),
            arguments: syn::PathArguments::None,
        });
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
                args: make_punc(Expr::Closure(syn::ExprClosure {
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

    let mut input_types = Punctuated::new();
    for arg in f.sig.inputs {
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
    let fn_once_bound = make_punc(syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: syn::Path {
            leading_colon: None,
            segments: make_punc(syn::PathSegment {
                ident: ident("Fn"),
                arguments: syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    paren_token: delim_token!(Paren),
                    inputs: input_types,
                    output: f.sig.output,
                }),
            }),
        },
    }));

    let check_fn = syn::Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::List(syn::MetaList {
                path: syn::Path {
                    leading_colon: None,
                    segments: make_punc_pathseg("inline"),
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
                params: make_punc(syn::GenericParam::Type(syn::TypeParam {
                    attrs: vec![],
                    ident: ident(FN_TYPE_NAME),
                    colon_token: Some(token!(Colon)),
                    bounds: fn_once_bound,
                    eq_token: None,
                    default: None,
                })),
                gt_token: Some(token!(Gt)),
                where_clause: None,
            },
            paren_token: delim_token!(Paren),
            inputs: make_punc(syn::FnArg::Typed(syn::PatType {
                attrs: vec![],
                pat: Box::new(syn::Pat::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: make_punc_pathseg(FN_ARG_NAME),
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
                            segments: make_punc_pathseg(FN_TYPE_NAME),
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
                        segments: make_punc(syn::PathSegment {
                            ident: ident("Option"),
                            arguments: syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: token!(Lt),
                                    args: make_punc(syn::GenericArgument::Type(
                                        syn::Type::Reference(syn::TypeReference {
                                            and_token: token!(And),
                                            lifetime: None,
                                            mutability: None,
                                            elem: Box::new(syn::Type::Path(syn::TypePath {
                                                qself: None,
                                                path: syn::Path {
                                                    leading_colon: None,
                                                    segments: make_punc_pathseg("str"),
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
            stmts: vec![Stmt::Expr(
                #[allow(clippy::unwrap_used)]
                pred.unwrap(),
                None,
            )],
        }),
    });

    let mut poirot_testify = make_punc_pathseg(crate::CRATE_NAME);
    poirot_testify.push(syn::PathSegment {
        ident: ident("testify"),
        arguments: syn::PathArguments::None,
    });
    let custom_mod = ident(&(f.sig.ident.to_string() + "_poirot"));
    let mut fn_to_test = make_punc_pathseg("super");
    fn_to_test.push(syn::PathSegment {
        ident: f.sig.ident,
        arguments: syn::PathArguments::None,
    });
    let test_fn = Item::Fn(syn::ItemFn {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::Path(syn::Path {
                leading_colon: None,
                segments: make_punc_pathseg(TEST_MACRO),
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
                            segments: poirot_testify,
                        },
                    })),
                    paren_token: delim_token!(Paren),
                    args: make_punc(Expr::Call(syn::ExprCall {
                        attrs: vec![],
                        func: Box::new(Expr::Path(syn::ExprPath {
                            attrs: vec![],
                            qself: None,
                            path: syn::Path {
                                leading_colon: None,
                                segments: make_punc_pathseg("check"),
                            },
                        })),
                        paren_token: delim_token!(Paren),
                        args: make_punc(Expr::Reference(syn::ExprReference {
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
    });

    syn::Item::Mod(syn::ItemMod {
        attrs: vec![syn::Attribute {
            pound_token: token!(Pound),
            style: syn::AttrStyle::Outer,
            bracket_token: delim_token!(Bracket),
            meta: syn::Meta::List(syn::MetaList {
                path: syn::Path {
                    leading_colon: None,
                    segments: make_punc_pathseg(CFG_MACRO),
                },
                delimiter: syn::MacroDelimiter::Paren(delim_token!(Paren)),
                tokens: ident("test").into_token_stream(),
            }),
        }],
        vis: syn::Visibility::Inherited,
        unsafety: None,
        mod_token: single_token!(Mod),
        ident: custom_mod,
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
    .to_tokens(&mut ts);

    Ok(ts)
}
