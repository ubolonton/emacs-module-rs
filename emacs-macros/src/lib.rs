//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;

use darling::FromMeta;
use quote::quote;
use syn::{self, AttributeArgs, export::TokenStream2, ItemFn, parse_macro_input};

use util::lisp_name;

mod util;
mod func;

#[derive(Debug, FromMeta)]
struct ModuleOpts {
    #[darling(default)]
    provide: Option<String>,
    #[darling(default)]
    prefix: Option<String>,
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
        None => lisp_name(&hook),
    };
    let prefix = match module_opts.prefix {
        Some(v) => v,
        None => format!("{}-", feature),
    };

    let init = quote!(__emacs_module_rs_auto_init__);
    let env = quote!(env);
    let register_init = quote! {
        ::emacs::emacs_module_init!(#init);
    };
    // TODO: Consider defining the map in user crate instead of `emacs`?
    let export_lisp_funcs = quote! {
        let funcs = ::emacs::func::__EMACS_MODULE_RS_AUTO_FUNCS__.lock()
            .expect("Fail to acquire a lock on map of initializers");
        for (name, func) in funcs.iter() {
            println!("Defining {}", name);
            func(#env)?
        }
    };
    let define_init = quote! {
        #[allow(non_snake_case)]
        fn #init(#env: &::emacs::Env) -> ::emacs::Result<::emacs::Value<'_>> {
            #export_lisp_funcs
            #hook(#env)?;
            #env.provide(#feature)
        }
    };
    // TODO: How about defining this in `emacs` crate?
    let define_prefix = quote! {
        static __EMACS_MODULE_RS_PREFIX__: &'static str = #prefix;
    };

    let tokens = quote! {
        #define_hook
        #define_prefix
        #define_init
        #register_init
    };
    tokens.into()
}

// This works even if the function is defined inside another function which is never called.
#[proc_macro_attribute]
pub fn func(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    // TODO: Add lifetime annotations so that input references have the same lifetime as &Env.
    let define_func: TokenStream2 = item_ts.clone().into();
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    let (func, errors) = func::parse(attr_args, fn_item);
    let (exporter, define_exporter) = func::gen_exporter(&func, errors);
    let register_exporter = func::gen_registrator(&func, exporter);
    let tokens = quote! {
        #define_func
        #define_exporter
        #register_exporter
    };
    tokens.into()
}
