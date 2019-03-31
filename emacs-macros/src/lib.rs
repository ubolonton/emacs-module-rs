//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{
    ItemFn, Ident,
    parse_macro_input,
    export::TokenStream2
};
use quote::quote;

/// Registers a function as the initialization hook, to be called when Emacs loads the module.
///
/// - Each dynamic module must have one and only one such function.
/// - The function's name will become the feature provided by the module. There is no need to
/// explicitly call `provide`.
#[proc_macro_attribute]
pub fn module(_attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let item_ts1 = item_ts.clone();
    let fn_item: ItemFn = parse_macro_input!(item_ts1);

    // TODO: Type-check this.
    let define_hook: TokenStream2 = item_ts.into();

    let hook = fn_item.ident;
    // TODO: Allow customizing module name.
    // TODO: Make sure module name is valid, and idiomatic.
    let module = hook.to_string();
    let init = Ident::new(&format!("__emacs_module_rs_init__{}", module), hook.span());
    let register_init = quote! {
        ::emacs::emacs_module_init!(#init);

        #[allow(non_snake_case)]
        fn #init(env: &::emacs::Env) -> ::emacs::Result<::emacs::Value<'_>> {
            #hook(env)?;
            env.provide(#module)
        }
    };

    let tokens = quote! {
        #define_hook
        #register_init
    };
    tokens.into()
}
