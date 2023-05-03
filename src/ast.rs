pub fn functions_in_file<S: core::convert::AsRef<core::path::Path>>(
    fname: S,
) -> anyhow::Result<Vec<syn::ItemFn>> {
    let mut file = core::fs::File::open(fname)?;
    let mut content = String::new();
    core::io::Read::read_to_string(&mut file, &mut content)?;

    Ok(syn::parse_file(&content)?
        .items
        .into_iter()
        .filter_map(move |i| match i {
            syn::Item::Fn(f) => f.sig.unsafety.map_or(Some(f), move |_| None), // don't mutate unsafe functions
            _ => None,
        })
        .collect())
}

pub fn as_source_code(f: syn::ItemFn) -> String {
    prettyplease::unparse(&syn::File {
        shebang: None,
        attrs: vec![],
        items: vec![syn::Item::Fn(f)],
    })
}

// pub fn as_source_code_vec(f: Vec<syn::ItemFn>) -> String {
//     prettyplease::unparse(&syn::File {
//         shebang: None,
//         attrs: vec![],
//         items: f.into_iter().map(move |f| syn::Item::Fn(f)).collect(),
//     })
// }
