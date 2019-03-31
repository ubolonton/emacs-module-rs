//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{
    ItemFn, parse_macro_input,
    export::TokenStream2
};
use quote::quote;

#[proc_macro_attribute]
pub fn module(_attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let item_ts1 = item_ts.clone();
    let fn_item: ItemFn = parse_macro_input!(item_ts1);

    let init = fn_item.ident;
    let register_init = quote! {
        ::emacs::emacs_module_init!(#init);
    };
    let define_init: TokenStream2 = item_ts.into();

    let tokens = quote! {
        #register_init
        #define_init
    };
    tokens.into()
}
