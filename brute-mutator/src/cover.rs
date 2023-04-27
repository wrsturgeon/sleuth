use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, Item};

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
fn make_path(i: Ident) -> Expr {
    Expr::Path(syn::ExprPath {
        attrs: vec![],
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: make_punc_pathseg(i),
        },
    })
}

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

        let this_pred = Expr::Call(syn::ExprCall {
            attrs: vec![],
            func: Box::new(make_path(fn_name)),
            paren_token: syn::token::Paren {
                span: proc_macro2::Group::new(
                    proc_macro2::Delimiter::Parenthesis,
                    TokenStream::new(),
                )
                .delim_span(),
            },
            args: punc_args,
        });

        pred = Some(if let Some(pred) = pred {
            Expr::Binary(syn::ExprBinary {
                attrs: vec![],
                left: Box::new(pred),
                op: syn::BinOp::And(syn::token::AndAnd {
                    spans: [Span::call_site(), Span::call_site()],
                }),
                right: Box::new(this_pred),
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

    let mut punc_generics = Punctuated::new();
    punc_generics.push_value(syn::GenericParam::Type(syn::TypeParam {
        attrs: vec![],
        ident: Ident::new("F", Span::call_site()),
        colon_token: None,
        bounds: Punctuated::new(),
        eq_token: None,
        default: None,
    }));
    let mut punc_type = Punctuated::new();
    punc_type.push_value(syn::PathSegment {
        ident: Ident::new("F", Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    let mut punc_inputs = Punctuated::new();
    punc_inputs.push_value(syn::FnArg::Typed(syn::PatType {
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
        ty: Box::new(syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: punc_type,
            },
        })),
    }));
    let mut punc_bool = Punctuated::new();
    punc_bool.push_value(syn::PathSegment {
        ident: Ident::new("bool", Span::call_site()),
        arguments: syn::PathArguments::None,
    });
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
                params: punc_generics,
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
            inputs: punc_inputs,
            variadic: None,
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
