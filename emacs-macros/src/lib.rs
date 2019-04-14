//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{self, AttributeArgs, ItemFn, parse_macro_input};

mod util;
mod module;
mod func;

/// Registers a function as the initialization hook, to be called when Emacs loads the module. Each
/// dynamic module must have one and only one such function.
///
/// # Options
///
/// - `name`: By default, name of the feature provided by the module is the crate's name (with `_`
/// replaced by `-`). There is no need to explicitly call `provide` inside the initialization hook.
/// This option allows the hook's name, or a string, to be used instead. For examples:
/// `#[module(name(fn))]`, or `#[module(name = "feature-name")]`.
/// - `separator`: Function names in Emacs are conventionally prefixed with the feature name,
/// followed by `-`, this option allows a different separator to be used. For example:
/// `#[module(separator = "/")]`.
/// - `mod_in_name`: Whether to put module path in function names. Default to `true`. This can also
/// be overridden for each individual function, by an option of the same name in [`#[defun]`].
///
/// [`#[defun]`]: attr.defun.html
#[proc_macro_attribute]
pub fn module(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    match module::Module::parse(attr_args, fn_item) {
        Ok(module) => module.render().into(),
        Err(e) => e.into(),
    }
}

/// Exports a function to the Lisp runtime. The function is bound at initialization, even if it is
/// defined inside another function which is never called.
///
/// # Naming
///
/// By default, the function's Lisp name has the form `<crate-prefix>[mod-prefix]<base-name>`.
/// - `crate-prefix` can be customized by the `name` and `separator` options on [`#[module]`].
/// - `mod-prefix` is constructed from the function's Rust module path (with `_` replaced by `-`).
/// This can be turned off crate-wide, or for individual function, using the option `mod_in_name`.
/// - `base-name` is the function's Rust name (with `_` replaced by `-`). This can be overridden
/// with the option `name`, e.g. `#[defun(name = "foo:bar")]`.
///
/// # Signature
///
/// - Each argument's type must be either:
///   + A type that implements [`FromLisp`]. Examples: `i64`, `String`, [`Value`].
///   + [`&Env`]. This is used to interact with the Lisp runtime. It does not appear in the
///   function's Lisp signature. This is unnecessary if there's already another argument with type
///   [`Value`].
/// - The return type must be [`Result<T>`], where `T` is a type that implements [`IntoLisp`].
/// Examples: `i64`, `String`, [`Value`], `impl IntoLisp`.
///
/// [`#[module]`]: attr.module.html
/// [`Result<T>`]: ../emacs/type.Result.html
/// [`FromLisp`]: ../emacs/trait.FromLisp.html
/// [`IntoLisp`]: ../emacs/trait.IntoLisp.html
/// [`&Env`]: ../emacs/struct.Env.html
/// [`Value`]: ../emacs/struct.Value.html
#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    match func::LispFunc::parse(attr_args, fn_item) {
        Ok(func) => func.render().into(),
        Err(e) => e.into(),
    }
}
