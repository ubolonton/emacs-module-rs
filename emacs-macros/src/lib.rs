//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{self, AttributeArgs, ItemFn, parse_macro_input};

mod util;
mod module;
mod func;

/// Registers a function as the initializer, to be called when Emacs loads the module. Each dynamic
/// module must have one and only one such function.
///
/// # Options
///
/// - `name`: By default, name of the feature provided by the module is the crate's name (with `_`
/// replaced by `-`). There is no need to explicitly call `provide` inside the initializer. This
/// option allows the initializer's name, or a string, to be used instead. For examples:
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

/// Exports a function to the Lisp runtime. The function is bound when the module is loaded, even if
/// it is defined inside another function which is never called.
///
/// # Input Parameters
///
/// Each parameter must be one of the following:
///
/// - An owned value of a type that implements [`FromLisp`]. This is for simple data types that have
/// an equivalent in Lisp. Examples: `i64`, `String`, `bool`.
///
/// - A shared/mutable reference. This gives access to data structures that other module functions
/// have created and embedded in the Lisp runtime (through `user-ptr` objects).
///
/// - A Lisp [`Value`]. This allows holding off the conversion to Rust data structures until
/// necessary, or working with values that don't have a meaningful representation in Rust, like Lisp
/// lambdas.
///
/// - An [`&Env`]. This enables interaction with the Lisp runtime. It does not appear in the
/// function's Lisp signature. This is unnecessary if there is already another parameter with type
/// [`Value`], which allows accessing the runtime through `Value.env`.
///
/// # Return Value
///
/// The return type must be [`Result<T>`], where `T` is one of the following:
///
/// - A type that implements [`IntoLisp`]. This is for simple data types that have an equivalent in
/// Lisp. Example: `i64`, `String`, `bool`.
///
/// - A type that implements `Transfer`. This allows embedding a native data structure in a
/// `user-ptr` object, for read-only use cases. It requires `user_ptr(direct)` option to be
/// specified.
///
/// - An arbitrary type. This allows embedding a native data structure in a `user-ptr` object, for
/// read-write use cases. It requires `user_ptr` option to be specified. If the data is to be shared
/// with background Rust threads, `user_ptr(rwlock)` or `user_ptr(mutex)` must be used instead.
///
/// - [`Value`]. This is mostly useful for returning an input parameter unchanged.
///
/// # Naming
///
/// By default, the function's Lisp name has the form `<feature-prefix>[mod-prefix]<base-name>`.
/// - `feature-prefix` is the feature's name, followed by `-`. This can be customized by the `name`
/// and `separator` options on [`#[module]`].
///
/// - `mod-prefix` is constructed from the function's Rust module path (with `_` replaced by `-`).
/// This can be turned off crate-wide, or for individual function, using the option `mod_in_name`.
///
/// - `base-name` is the function's Rust name (with `_` replaced by `-`). This can be overridden
/// with the option `name`, e.g. `#[defun(name = "foo:bar")]`.
///
/// [`#[module]`]: attr.module.html
/// [`Result<T>`]: ../emacs/type.Result.html
/// [`FromLisp`]: ../emacs/trait.FromLisp.html
/// [`IntoLisp`]: ../emacs/trait.IntoLisp.html
/// [`Transfer`]: ../emacs/trait.Transfer.html
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
