use std::cell::RefCell;
use std::collections::HashMap;

use emacs::{defun, Env, Error, ErrorKind, LispType, ModuleError, Result, RustError, Value};

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn handle_signal(env: &Env, some_text: Value<'_>) -> Result<()> {
// ANCHOR: handle_signal
match env.call("insert", [some_text]) {
    Err(error) => {
        // Handle `buffer-read-only` error.
        if let Some(ErrorKind::Signal { symbol, .. }) = error.downcast_ref::<ErrorKind>() {
            let buffer_read_only = env.intern("buffer-read-only")?;
            // `symbol` is a `TempValue` that must be converted to `Value`.
            let symbol = unsafe { symbol.value(env) };
            if symbol == buffer_read_only {
                env.message("This buffer is not writable!")?;
                return Ok(());
            }
        }
        // Propagate other errors.
        Err(error)
    }
    Ok(_) => Ok(()),
}
// ANCHOR_END: handle_signal
}

fn classify(error: &Error) {
// ANCHOR: classify
match error.downcast_ref::<ErrorKind>() {
    Some(ErrorKind::Signal { .. } | ErrorKind::Throw { .. }) => {
        // Lisp code signaled or threw.
    }
    Some(ErrorKind::Module(_)) => {
        // The module layer rejected the call.
    }
    Some(ErrorKind::Rust(_)) => {
        // Rust code in this crate rejected the value.
    }
    None => {
        // Not an `ErrorKind`.
    }
}
// ANCHOR_END: classify
}

fn match_variants(env: &Env, error: Error) -> Result<()> {
// ANCHOR: match_variants
match error.downcast_ref::<ErrorKind>() {
    Some(ErrorKind::Module(ModuleError::WrongType { expected: LispType::Integer, .. })) => {
        env.message("Expected an integer")?;
    }
    Some(ErrorKind::Rust(RustError::InvalidUtf8 { .. })) => {
        env.message("Not valid UTF-8")?;
    }
    // `ModuleError` and `RustError` can grow new variants in a minor release.
    _ => return Err(error),
}
// ANCHOR_END: match_variants
Ok(())
}

// ANCHOR: define_errors
// The parentheses denote parent error signals.
// If unspecified, the parent error signal is `error`.
emacs::define_errors! {
    my_custom_error "This number should not be negative" (arith_error range_error)
}

#[defun]
fn signal_if_negative(env: &Env, x: i16) -> Result<()> {
    if x < 0 {
        return env.signal(my_custom_error, ("associated", "DATA", 7))
    }
    Ok(())
}
// ANCHOR_END: define_errors

fn wrong_type_user_ptr(value: Value<'_>) -> Result<()> {
// ANCHOR: wrong_type_user_ptr
// May signal `rust-wrong-type-user-ptr` if `value` holds a different type of hash map,
// or is a `user-ptr` defined in a non-Rust module.
let r: &RefCell<HashMap<String, String>> = value.into_rust()?;
// ANCHOR_END: wrong_type_user_ptr
Ok(())
}
