use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item};

pub fn cover_impl(attr: TokenStream, input: TokenStream) -> Result<TokenStream, syn::Error> {
    #![allow(unused_assignments)]

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

    // Make sure this macro is applied to a function, and get the function's AST
    let Item::Fn(f) = ast else { return Err(syn::Error::new(ast.span(), "This macro can be applied only to functions")) };
    if f.sig.unsafety.is_some() {
        return Err(syn::Error::new(
            f.sig.unsafety.span(),
            "Please don't recklessly mutate unsafe functions",
        ));
    }

    // Iterate through tests to cover
    #[allow(unused_variables)]
    let mut pred: Option<Box<Expr>> = None;
    let mut attr_iter = attr.into_iter();
    while let Some(maybe_fnname) = attr_iter.next() {
        // Make sure we have a valid function-call syntax
        let proc_macro2::TokenTree::Ident(_fnname) = maybe_fnname else {
            return Err(syn::Error::new(maybe_fnname.span(), "This macro takes a set of function calls: ..., func(a, b), other_func(b, c, d), ..."));
        };
        let maybe_fnargs = attr_iter.next();
        let Some(proc_macro2::TokenTree::Group(fnargs)) = maybe_fnargs else {
            return Err(syn::Error::new(maybe_fnargs.span(), "This macro takes a set of function calls: ..., func(a, b), other_func(b, c, d), ..."));
        };

        // Write a function call and append it to the chain of `&&`s
        let mut fn_name_segs = Punctuated::new();
        fn_name_segs.push_value(syn::PathSegment {
            ident: syn::Ident::new(&_fnname.to_string(), Span::call_site()),
            arguments: syn::PathArguments::None,
        });
        let mut fn_arg_inner_segs = Punctuated::new();
        fn_arg_inner_segs.push_value(syn::PathSegment {
            ident: syn::Ident::new("f", Span::call_site()),
            arguments: syn::PathArguments::None,
        });
        let mut fn_arg_segs = Punctuated::new();
        fn_arg_segs.push_value(syn::Expr::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: fn_arg_inner_segs,
            },
        }));
        for whatever in fnargs.stream() {
            fn_arg_segs.push_punct(syn::token::Comma {
                spans: [Span::call_site()],
            });
            match whatever {
                proc_macro2::TokenTree::Ident(i) => {
                    let mut punc = Punctuated::new();
                    punc.push(syn::PathSegment {
                        ident: i.into(),
                        arguments: syn::PathArguments::None,
                    });
                    fn_arg_segs.push_value(syn::Expr::Path(syn::ExprPath {
                        attrs: vec![],
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: punc,
                        },
                    }));
                }
                proc_macro2::TokenTree::Literal(i) => {
                    fn_arg_segs.push_value(syn::Expr::Lit(syn::ExprLit {
                        attrs: vec![],
                        lit: syn::Lit::Verbatim(i),
                    }));
                }
                _ => {
                    return Err(syn::Error::new(
                        whatever.span(),
                        "Seems like an invalid argument to a test predicate",
                    ))
                }
            };
        }
        #[allow(unused_variables)]
        let this_pred = Box::new(Expr::Call(syn::ExprCall {
            attrs: vec![],
            func: Box::new(Expr::Path(syn::ExprPath {
                attrs: vec![],
                qself: None,
                path: syn::Path {
                    leading_colon: None,
                    segments: fn_name_segs,
                },
            })),
            paren_token: syn::token::Paren {
                span: proc_macro2::Group::new(
                    proc_macro2::Delimiter::Parenthesis,
                    TokenStream::new(),
                )
                .delim_span(),
            },
            args: fn_arg_segs,
        }));
        pred = Some(if let Some(pred) = pred {
            Box::new(Expr::Binary(syn::ExprBinary {
                attrs: vec![],
                left: pred,
                op: syn::BinOp::And(syn::token::AndAnd {
                    spans: [Span::call_site(), Span::call_site()],
                }),
                right: this_pred,
            }))
        } else {
            this_pred
        });

        let maybe_comma = attr_iter.next();
        if maybe_comma.is_none() {
            break;
        }
        if let Some(proc_macro2::TokenTree::Punct(ref comma)) = maybe_comma {
            if comma.as_char() != ',' {
                return Err(syn::Error::new(maybe_comma.span(), "This macro takes a set of function calls: ..., func(a, b), other_func(b, c, d), ..."));
            }
        } else {
            return Err(syn::Error::new(maybe_comma.span(), "This macro takes a set of function calls: ..., func(a, b), other_func(b, c, d), ..."));
        }

        // Add tests
    }

    // Create a new function that will tell us whether all tests are covered
    let mut punc_bool = Punctuated::new();
    punc_bool.push_value(syn::PathSegment {
        ident: syn::Ident::new("bool", Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    let mut punc_generics = Punctuated::new();
    punc_generics.push(syn::GenericParam::Type(syn::TypeParam {
        attrs: vec![],
        ident: syn::Ident::new("F", Span::call_site()),
        colon_token: None,
        bounds: Punctuated::new(),
        eq_token: None,
        default: None,
    }));
    let mut punc_argnames = Punctuated::new();
    punc_argnames.push(syn::PathSegment {
        ident: syn::Ident::new("f", Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    let mut punc_argtypes = Punctuated::new();
    punc_argtypes.push(syn::PathSegment {
        ident: syn::Ident::new("F", Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    let mut punc_args = Punctuated::new();
    punc_args.push(syn::FnArg::Typed(syn::PatType {
        attrs: vec![],
        pat: Box::new(syn::Pat::Path(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: punc_argtypes,
            },
        })),
        colon_token: syn::token::Colon {
            spans: [Span::call_site()],
        },
        ty: Box::new(syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: punc_argnames,
            },
        })),
    }));
    syn::Item::Fn(syn::ItemFn {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            ident: proc_macro2::Ident::new(
                &(f.sig.ident.to_string() + "_check_cover"),
                Span::call_site(),
            ),
            generics: syn::Generics {
                lt_token: Some(syn::token::Lt {
                    spans: [Span::call_site()],
                }),
                params: punc_generics,
                gt_token: Some(syn::token::Gt {
                    spans: [Span::call_site()],
                }),
                where_clause: None,
            },
            inputs: punc_args,
            output: syn::ReturnType::Type(
                syn::token::RArrow {
                    spans: [Span::call_site(), Span::call_site()],
                },
                Box::new(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: punc_bool,
                    },
                })),
            ),
            ..f.sig
        },
        block: Box::new(syn::Block {
            brace_token: syn::token::Brace {
                span: proc_macro2::Group::new(proc_macro2::Delimiter::Brace, TokenStream::new())
                    .delim_span(),
            },
            stmts: vec![syn::Stmt::Expr(*pred.unwrap(), None)],
        }),
    })
    .to_tokens(&mut ts);

    Ok(ts)
}
