use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::spanned::Spanned;

pub fn fn_predicate_impl(attr: TokenStream, input: TokenStream) -> Result<TokenStream, syn::Error> {
    if !attr.is_empty() {
        return Err(syn::Error::new(
            attr.span(),
            "This macro takes no arguments",
        ));
    }

    let ast = syn::parse2(input)?;
    let syn::Item::Fn(f) = ast else { return Err(syn::Error::new(ast.span(), "This macro can be applied only to functions")) };

    Ok(syn::Item::Fn(f).into_token_stream())
}
