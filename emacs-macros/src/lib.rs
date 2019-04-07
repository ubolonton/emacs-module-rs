//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{self, AttributeArgs, ItemFn, parse_macro_input};

mod util;
mod module;
mod func;

/// Registers a function as the initialization hook, to be called when Emacs loads the module.
///
/// - Each dynamic module must have one and only one such function.
/// - The function name will become name of the feature provided by the module. There is no need
/// to explicitly call `provide` inside the initialization hook. The feature name can be overridden
/// by the `name` option, e.g. `#[module(name = "feature-name")]`.
#[proc_macro_attribute]
pub fn module(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    match module::Module::parse(attr_args, fn_item) {
        Ok(module) => module.render().into(),
        Err(e) => e.into(),
    }
}

// This works even if the function is defined inside another function which is never called.
#[proc_macro_attribute]
pub fn func(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    match func::LispFunc::parse(attr_args, fn_item) {
        Ok(func) => func.render().into(),
        Err(e) => e.into(),
    }
}
