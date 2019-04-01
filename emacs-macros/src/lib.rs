//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{
    ItemFn, AttributeArgs,
    parse_macro_input,
    export::TokenStream2
};
use quote::quote;
use darling::FromMeta;

#[derive(Debug, FromMeta)]
struct ModuleOpts {
    #[darling(default)]
    provide: Option<String>
}

/// Registers a function as the initialization hook, to be called when Emacs loads the module.
///
/// - Each dynamic module must have one and only one such function.
/// - The function name will become name of the feature provided by the module. There is no need
/// to explicitly call `provide` inside the initialization hook. The feature name can be overridden
/// by the `provide` option, e.g. `#[module(provide = "feature-name")]`.
#[proc_macro_attribute]
pub fn module(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let module_opts: ModuleOpts = match ModuleOpts::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => return e.write_errors().into(),
    };

    // TODO: Type-check this.
    let define_hook: TokenStream2 = item_ts.clone().into();

    let fn_item: ItemFn = parse_macro_input!(item_ts);
    let hook = fn_item.ident;
    let feature = match module_opts.provide {
        Some(v) => v,
        // The trimming is a workaround for https://github.com/dtolnay/syn/issues/478.
        None => lisp_name(hook.to_string().trim_start_matches("r#")),
    };
    let init = quote!(__emacs_module_rs_auto_init__);
    let register_init = quote! {
        ::emacs::emacs_module_init!(#init);

        #[allow(non_snake_case)]
        fn #init(env: &::emacs::Env) -> ::emacs::Result<::emacs::Value<'_>> {
            #hook(env)?;
            env.provide(#feature)
        }
    };

    let tokens = quote! {
        #define_hook
        #register_init
    };
    tokens.into()
}

// TODO: Add more extensively checks and transformations to make this more "idiomatic".
fn lisp_name(name: &str) -> String {
    name.replace("_", "-")
}
