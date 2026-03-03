//! Testing `PartialEq` for `Value` and `GlobalRef`.

use emacs::{defun, use_symbols, Env, GlobalRef, IntoLisp, Result, Value};

// Symbols representing the three directions a node can appear in.
use_symbols! {
    left right center
}

/// Return t if A and B are `eq` (using Rust's `==` operator on `Value`).
#[defun(mod_in_name = false, name = "eq:value-eq")]
fn value_eq<'e>(a: Value<'e>, b: Value<'e>) -> Result<Value<'e>> {
    let env = a.env;
    (a == b).into_lisp(env)
}

/// Return t if GLOBAL-REF is `eq` to VALUE (using Rust's `==` on `GlobalRef`).
#[defun(mod_in_name = false, name = "eq:global-ref-eq")]
fn global_ref_eq<'e>(global: GlobalRef, value: Value<'e>) -> Result<Value<'e>> {
    let env = value.env;
    (global == value).into_lisp(env)
}

/// Return a newly allocated string with the given content, for testing that equal
/// strings are not `eq`.
#[defun(mod_in_name = false, name = "eq:new-string")]
fn new_string(env: &Env, s: String) -> Result<Value<'_>> {
    s.into_lisp(env)
}

/// Classify a node's POSITION symbol as one of "left", "right", or "center".
/// Returns an error string for unrecognized symbols.
///
/// This illustrates the idiomatic use of `PartialEq` on `Value`: comparing an
/// incoming argument against `use_symbols!`-imported `OnceGlobalRef` globals
/// avoiding repeated `intern` calls on the hot path.
#[defun(mod_in_name = false, name = "eq:classify-position")]
fn classify_position(position: Value<'_>) -> Result<String> {
    if position == *left {
        Ok("left".to_owned())
    } else if *right == position {
        Ok("right".to_owned())
    } else if *center == position {
        Ok("center".to_owned())
    } else {
        Ok("unknown".to_owned())
    }
}

pub fn init(_env: &Env) -> Result<()> {
    Ok(())
}
