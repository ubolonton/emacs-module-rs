use std::fmt::Display;

use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    export::{Span, TokenStream2},
    Ident, ItemFn,
};

// TODO: Add more extensively checks and transformations to make this more "idiomatic".
pub fn lisp_name(id: &Ident) -> String {
    id.to_string().trim_start_matches("r#").replace("_", "-")
}

pub fn concat(lhs: &str, rhs: &Ident) -> Ident {
    Ident::new(&format!("{}{}", lhs, rhs), Span::call_site())
}

pub fn arg(name: &str, i: usize) -> Ident {
    Ident::new(&format!("{}{}", name, i), Span::call_site())
}

pub fn report<T: ToTokens, U: Display>(errors: &mut TokenStream2, ts: T, msg: U) {
    errors.append_all(syn::Error::new_spanned(ts, msg).to_compile_error());
}

// TODO: Report errors.
// TODO: Use syn::Parse?
pub fn doc(fn_item: &ItemFn) -> String {
    let doc = &mut vec![];
    for attr in &fn_item.attrs {
        if let Ok(syn::Meta::NameValue(mnv)) = attr.parse_meta() {
            if mnv.ident == "doc" {
                if let syn::Lit::Str(ls) = mnv.lit {
                    doc.push(ls.value().trim_start_matches(' ').to_owned());
                }
            }
        }
    }
    doc.join("\n")
}

pub fn init_fns_path() -> TokenStream2 {
    quote!(::emacs::globals::__INIT_FNS__)
}

pub fn prefix_path() -> TokenStream2 {
    quote!(::emacs::globals::__PREFIX__)
}

pub fn mod_in_name_path() -> TokenStream2 {
    quote!(::emacs::globals::__MOD_IN_NAME__)
}
