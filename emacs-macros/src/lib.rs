//! This crate implements proc macros for the `emacs` crate. It is a dependency of `emacs` crate,
//! and should not be listed as a direct dependency by Emacs dynamic modules.

#![recursion_limit = "128"]

extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{self, AttributeArgs, ItemFn, LitInt, parse_macro_input};

mod util;
mod module;
mod func;
mod lisp_args;

/// Registers a function as the initializer, to be called when Emacs loads the module. Each dynamic
/// module must have one and only one such function.
///
/// # Options
///
/// - `name`: By default, the name of the feature provided by the module is the crate's name (with
/// `_` replaced by `-`). There is no need to explicitly call `provide` inside the initializer. This
/// option allows the initializer's name, or a string, to be used instead. Examples:
/// `#[module(name(fn))]`, `#[module(name = "feature-name")]`.
///
/// - `defun_prefix` and `separator`: Function names in Emacs are conventionally prefixed with the
/// feature name followed by `-`. These 2 options allow different prefix and separator to be used.
/// Example: `#[module(name = "foo-dyn", defun_prefix = "foo", separator = "/")]`.
///
/// - `mod_in_name`: Whether to use Rust's `mod` path to construct function names. Default to
/// `true`. For example, supposed that the crate is named `parser`, a #[[`defun`]] named `next_child`
/// inside `mod cursor` will have the Lisp name of `parser-cursor-next-child`. This can also be
/// overridden for each individual function, by an option of the same name on #[[`defun`]].
///
/// [`defun`]: attr.defun.html
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
/// - A Lisp [`Value`], or one of its "sub-types" (e.g. [`Vector`]). This allows holding off the
/// conversion to Rust data structures until necessary, or working with values that don't have a
/// meaningful representation in Rust, like Lisp lambdas.
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
/// - [`Value`], or one of its "sub-types" (e.g. [`Vector`]). This is mostly useful for returning an
/// input parameter unchanged.
///
/// # Naming
///
/// By default, the function's Lisp name has the form `<crate-prefix>[mod-prefix]<base-name>`.
///
/// - `crate-prefix` is the feature name followed by `-`. This can be customized by the `name`,
/// `defun_prefix`, and `separator` options on #[[`module`]].
///
/// - `mod-prefix` is constructed from the function's Rust module path (with `_` and `::` replaced
/// by `-`). This can be turned off crate-wide, or for individual function, using the option
/// `mod_in_name`.
///
/// - `base-name` is the function's Rust name (with `_` replaced by `-`). This can be overridden
/// with the option `name`, e.g. `#[defun(name = "foo:bar")]`.
///
/// [`module`]: attr.module.html
/// [`Result<T>`]: /emacs/*/emacs/type.Result.html
/// [`FromLisp`]: /emacs/*/emacs/trait.FromLisp.html
/// [`IntoLisp`]: /emacs/*/emacs/trait.IntoLisp.html
/// [`Transfer`]: /emacs/*/emacs/trait.Transfer.html
/// [`&Env`]: /emacs/*/emacs/struct.Env.html
/// [`Value`]: /emacs/*/emacs/struct.Value.html
/// [`Vector`]: /emacs/*/emacs/struct.Vector.html
#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, item_ts: TokenStream) -> TokenStream {
    let attr_args: AttributeArgs = parse_macro_input!(attr_ts);
    let fn_item: ItemFn = parse_macro_input!(item_ts);
    match func::LispFunc::parse(attr_args, fn_item) {
        Ok(func) => func.render().into(),
        Err(e) => e.into(),
    }
}

#[proc_macro]
pub fn impl_lisp_args_for_tuples_with_max_arity(arity: TokenStream) -> TokenStream {
    let arity: LitInt = parse_macro_input!(arity);
    lisp_args::impl_for_tuples(arity.value() as usize).into()
}
