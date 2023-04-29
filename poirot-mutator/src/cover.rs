use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item};

const FN_TYPE_NAME: &'static str = "_FnToCheck";
const FN_ARG_NAME: &'static str = "f";

#[inline(always)]
fn make_punc<T, P>(v: T) -> Punctuated<T, P> {
    let mut punc = Punctuated::new();
    punc.push_value(v);
    punc
}

#[inline(always)]
fn make_punc_pathseg<P>(i: Ident) -> Punctuated<syn::PathSegment, P> {
    make_punc(syn::PathSegment {
        ident: i,
        arguments: syn::PathArguments::None,
    })
}

#[inline(always)]
pub fn cover_impl(attr: TokenStream, input: TokenStream) -> Result<TokenStream, syn::Error> {
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
            "Please don't recklessly mutate unsafe functions",
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
                segments: make_punc_pathseg(Ident::new(FN_ARG_NAME, Span::call_site())),
            },
        }));
        punc_args.push(Expr::Verbatim(fn_args.stream()));

        let mut fn_path = make_punc_pathseg(Ident::new("crate", Span::call_site()));
        fn_path.push(syn::PathSegment {
            ident: Ident::new("poirot", Span::call_site()),
            arguments: syn::PathArguments::None,
        });
        fn_path.push(syn::PathSegment {
            ident: fn_name,
            arguments: syn::PathArguments::None,
        });
        let this_pred = syn::Expr::Macro(syn::ExprMacro {
            attrs: vec![],
            mac: syn::Macro {
                path: syn::Path {
                    leading_colon: None,
                    segments: make_punc_pathseg(Ident::new("timid_assert", Span::call_site())),
                },
                bang_token: syn::token::Not {
                    spans: [Span::call_site()],
                },
                delimiter: syn::MacroDelimiter::Paren(syn::token::Paren {
                    span: proc_macro2::Group::new(
                        proc_macro2::Delimiter::Parenthesis,
                        TokenStream::new(),
                    )
                    .delim_span(),
                }),
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
                    paren_token: syn::token::Paren {
                        span: proc_macro2::Group::new(
                            proc_macro2::Delimiter::Parenthesis,
                            TokenStream::new(),
                        )
                        .delim_span(),
                    },
                    args: punc_args,
                })
                .into_token_stream(),
            },
        });

        pred = Some(if let Some(pred) = pred {
            Expr::MethodCall(syn::ExprMethodCall {
                attrs: vec![],
                receiver: Box::new(pred),
                dot_token: syn::token::Dot {
                    spans: [Span::call_site()],
                },
                method: Ident::new("or_else", Span::call_site()),
                turbofish: None,
                paren_token: syn::token::Paren {
                    span: proc_macro2::Group::new(
                        proc_macro2::Delimiter::Parenthesis,
                        TokenStream::new(),
                    )
                    .delim_span(),
                },
                args: make_punc(Expr::Closure(syn::ExprClosure {
                    attrs: vec![],
                    lifetimes: None,
                    constness: None,
                    movability: None,
                    asyncness: None,
                    capture: None,
                    or1_token: syn::token::Or {
                        spans: [Span::call_site()],
                    },
                    inputs: Punctuated::new(),
                    or2_token: syn::token::Or {
                        spans: [Span::call_site()],
                    },
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
                ident: Ident::new("Fn", Span::call_site()),
                arguments: syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    paren_token: syn::token::Paren {
                        span: proc_macro2::Group::new(
                            proc_macro2::Delimiter::Parenthesis,
                            TokenStream::new(),
                        )
                        .delim_span(),
                    },
                    inputs: input_types,
                    output: f.sig.output,
                }),
            }),
        },
    }));
    syn::Item::Fn(syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: syn::token::Fn {
                span: Span::call_site(),
            },
            ident: Ident::new(
                &(f.sig.ident.to_string() + "_check_cover"),
                Span::call_site(),
            ),
            generics: syn::Generics {
                lt_token: Some(syn::token::Lt {
                    spans: [Span::call_site()],
                }),
                params: make_punc(syn::GenericParam::Type(syn::TypeParam {
                    attrs: vec![],
                    ident: Ident::new(FN_TYPE_NAME, Span::call_site()),
                    colon_token: Some(syn::token::Colon {
                        spans: [Span::call_site()],
                    }),
                    bounds: fn_once_bound,
                    eq_token: None,
                    default: None,
                })),
                gt_token: Some(syn::token::Gt {
                    spans: [Span::call_site()],
                }),
                where_clause: None,
            },
            paren_token: syn::token::Paren {
                span: proc_macro2::Group::new(
                    proc_macro2::Delimiter::Parenthesis,
                    TokenStream::new(),
                )
                .delim_span(),
            },
            inputs: make_punc(syn::FnArg::Typed(syn::PatType {
                attrs: vec![],
                pat: Box::new(syn::Pat::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: make_punc_pathseg(Ident::new(FN_ARG_NAME, Span::call_site())),
                    },
                })),
                colon_token: syn::token::Colon {
                    spans: [Span::call_site()],
                },
                ty: Box::new(syn::Type::Reference(syn::TypeReference {
                    and_token: syn::token::And {
                        spans: [Span::call_site()],
                    },
                    lifetime: None,
                    mutability: None,
                    elem: Box::new(syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: make_punc_pathseg(Ident::new(
                                FN_TYPE_NAME,
                                Span::call_site(),
                            )),
                        },
                    })),
                })),
            })),
            variadic: None,
            output: syn::ReturnType::Type(
                syn::token::RArrow {
                    spans: [Span::call_site(), Span::call_site()],
                },
                Box::new(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: make_punc(syn::PathSegment {
                            ident: Ident::new("Option", Span::call_site()),
                            arguments: syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: syn::token::Lt {
                                        spans: [Span::call_site()],
                                    },
                                    args: make_punc(syn::GenericArgument::Type(
                                        syn::Type::Reference(syn::TypeReference {
                                            and_token: syn::token::And {
                                                spans: [Span::call_site()],
                                            },
                                            lifetime: None,
                                            mutability: None,
                                            elem: Box::new(syn::Type::Path(syn::TypePath {
                                                qself: None,
                                                path: syn::Path {
                                                    leading_colon: None,
                                                    segments: make_punc_pathseg(Ident::new(
                                                        "str",
                                                        Span::call_site(),
                                                    )),
                                                },
                                            })),
                                        }),
                                    )),
                                    gt_token: syn::token::Gt {
                                        spans: [Span::call_site()],
                                    },
                                },
                            ),
                        }),
                    },
                })),
            ),
        },
        block: Box::new(syn::Block {
            brace_token: syn::token::Brace {
                span: proc_macro2::Group::new(proc_macro2::Delimiter::Brace, TokenStream::new())
                    .delim_span(),
            },
            stmts: vec![syn::Stmt::Expr(pred.unwrap(), None)],
        }),
    })
    .to_tokens(&mut ts);

    Ok(ts)
}
