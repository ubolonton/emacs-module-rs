//! Records where each #[[`defun`]] is defined, so that Lisp tools can find its Rust source. It
//! should be mainly used by the macros, not module code.
//!
//! The record is a symbol property: `(get SYMBOL 'emacs-module-rs-source)` is `(FILE LINE
//! FEATURE)`, where FILE is relative to the crate's root. Lisp code resolves FILE against
//! `(get FEATURE 'emacs-module-rs-source-root)`, which the package that ships the module sets. The
//! module does not set it, because the build directory is not where the package installs the
//! sources.
//!
//! `#[module(source_lookup)]` makes the module install [`SOURCE_LOOKUP`], which lets
//! `find-function` use the record.
//!
//! [`defun`]: attr.defun.html

use crate::{Env, IntoLisp, Result, Value, init::__FEATURE__};

/// Symbol property that holds a module function's `(FILE LINE FEATURE)`.
pub const SOURCE_PROPERTY: &str = "emacs-module-rs-source";

/// Records that the function `name` is defined at `line` of `file`, which is relative to the
/// crate's root.
pub fn __record(env: &Env, name: &str, file: &str, line: u32) -> Result<()> {
    let feature = __FEATURE__.try_lock().expect("Failed to acquire read lock of module feature");
    let location = env.list((file, line, env.intern(&feature)?))?;
    env.call("put", (env.intern(name)?, env.intern(SOURCE_PROPERTY)?, location))?;
    Ok(())
}

/// Lisp code that advises `find-function-search-for-symbol` to visit the recorded source.
pub const SOURCE_LOOKUP: &str = include_str!("source_lookup.el");

/// Evaluates [`SOURCE_LOOKUP`].
pub fn __install_lookup(env: &Env) -> Result<()> {
    // `read-from-string` reads one form. The newline ends a trailing comment, if any.
    let code = format!("(progn {SOURCE_LOOKUP}\n)");
    let form = env.call("read-from-string", [code.into_lisp(env)?])?.car::<Value>()?;
    env.call("eval", (form, true))?;
    Ok(())
}
