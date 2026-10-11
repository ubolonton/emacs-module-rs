//! Records where each #[[`defun`]] is defined, so that Lisp tools can find its Rust source. It
//! should be mainly used by the macros, not module code.
//!
//! The record is a symbol property: `(get SYMBOL 'emacs-module-rs-source)` is `(FILE LINE
//! FEATURE)`, where FILE is relative to the crate's root. Lisp code resolves FILE against
//! `(get FEATURE 'emacs-module-rs-source-root)`, which the package that ships the module sets. The
//! module does not set it, because the build directory is not where the package installs the
//! sources.
//!
//! [`defun`]: attr.defun.html

use crate::{Env, Result, init::__FEATURE__};

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
