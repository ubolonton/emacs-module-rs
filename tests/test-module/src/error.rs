//! Testing error reporting and handling.

use std::{cell::RefCell, fs, num::NonZeroU8};

use emacs::{defun, CallEnv, Env, IntoLisp, ModuleError, Result, RustError, Value, Vector};
use emacs::ErrorKind::{self, Signal, Throw};
use emacs::ResultExt;

use super::MODULE_PREFIX;

#[defun(mod_in_name = false, name = "error:lisp-divide")]
fn lisp_divide(x: Value<'_>, y: Value<'_>) -> Result<i64> {
    fn inner(env: &Env, x: i64, y: i64) -> Result<Value<'_>> {
        call!(env, "/", x, y)
    }

    fn foo<'e>(env: &'e Env, x: Value<'_>, y: Value<'_>) -> Result<Value<'e>> {
        inner(
            env,
            x.into_rust()?,
            y.into_rust()?,
        )
    }

    foo(x.env, x, y)?.into_rust()
}

#[defun(mod_in_name = false, name = "error:get-type")]
fn get_type(f: Value<'_>) -> Result<Value<'_>> {
    let env = f.env;
    match f.call([]) {
        Err(error) => {
            if let Some(Signal { symbol, .. }) = error.downcast_ref::<ErrorKind>() {
                unsafe {
                    return Ok(symbol.value(env));
                }
            }
            Err(error)
        }
        v => v,
    }
}

/// Call LAMBDA and return the result. Return the thrown value if EXPECTED-TAG is thrown.
#[defun(mod_in_name = false, name = "error:catch")]
fn catch<'e>(expected_tag: Value<'e>, lambda: Value<'e>) -> Result<Value<'e>> {
    let env = expected_tag.env;
    match lambda.call([]) {
        Err(error) => {
            if let Some(Throw { tag, value }) = error.downcast_ref::<ErrorKind>() {
                unsafe {
                    if tag.value(env) == expected_tag {
                        return Ok(value.value(env));
                    }
                }
            }
            Err(error)
        }
        v => v,
    }
}

/// Call `apply` on LAMBDA and ARGS, propagating any signaled error.
#[defun(mod_in_name = false, name = "error:apply")]
fn apply<'e>(lambda: Value<'e>, args: Value<'e>) -> Result<Value<'e>> {
    let env = lambda.env;
    env.call("apply", (lambda, args))
}

/// Apply OPERATION to VALUE, and to EXTRA if the operation needs it. Return the path of the
/// resulting `ErrorKind` variant, e.g. "Module/WrongType/Integer". Return nil if there is no error.
#[defun(mod_in_name = false, name = "error:variant")]
fn variant<'e>(
    env: &'e Env,
    operation: String,
    value: Value<'e>,
    extra: Value<'e>,
) -> Result<Option<String>> {
    let result = (|| -> Result<()> {
        match operation.as_str() {
            "funcall" => { value.call([])?; }
            "i64" => { value.into_rust::<i64>()?; }
            "u8" => { value.into_rust::<u8>()?; }
            "nonzero-u8" => { value.into_rust::<NonZeroU8>()?; }
            "f64" => { value.into_rust::<f64>()?; }
            "string" => { value.into_rust::<String>()?; }
            "bytes" => { value.clone_string_contents()?; }
            "vector" => { value.into_rust::<Vector>()?; }
            "vec-get" => {
                let index: i64 = extra.into_rust()?;
                value.into_rust::<Vector>()?.get::<Value>(index as usize)?;
            }
            // This index is larger than any fixnum.
            "vec-get-far" => { value.into_rust::<Vector>()?.get::<Value>(1 << 62)?; }
            "copy-string-contents" => {
                let mut buffer = vec![0u8; extra.into_rust()?];
                value.copy_string_contents(&mut buffer)?;
            }
            "ref-cell" => { value.into_rust::<&RefCell<i64>>()?; }
            "i64-max-into-lisp" => { i64::MAX.into_lisp(env)?; }
            "u64-max-into-lisp" => { u64::MAX.into_lisp(env)?; }
            _ => return Err(emacs::Error::msg(format!("Unknown operation: {operation}"))),
        }
        Ok(())
    })();
    Ok(result.err().map(|error| describe(&error)))
}

/// Convert VALUE to an `i64`, then propagate the resulting `ModuleError` on its own, without the
/// `ErrorKind` wrapper around it. This simulates user code that takes the inner error out of an
/// `ErrorKind` (for example, to inspect it) and re-propagates it with `?`.
#[defun(mod_in_name = false, name = "error:propagate-bare-module-error")]
fn propagate_bare_module_error(value: Value<'_>) -> Result<i64> {
    match value.into_rust::<i64>() {
        Ok(i) => Ok(i),
        Err(error) => match error.downcast::<ErrorKind>() {
            Ok(ErrorKind::Module(module_error)) => Err(module_error.into()),
            Ok(other) => Err(other.into()),
            Err(error) => Err(error),
        },
    }
}

/// Return the variant path of ERROR, e.g. "Module/WrongType/Integer".
fn describe(error: &emacs::Error) -> String {
    // `ErrorKind` is exhaustive, so this match must not need a `_` arm.
    match error.downcast_ref::<ErrorKind>() {
        None => format!("Other: {error}"),
        Some(Signal { .. }) => "Signal".to_string(),
        Some(Throw { .. }) => "Throw".to_string(),
        Some(ErrorKind::Module(error)) => format!("Module/{}", describe_module(error)),
        Some(ErrorKind::Rust(error)) => format!("Rust/{}", describe_rust(error)),
    }
}

fn describe_module(error: &ModuleError) -> String {
    match error {
        ModuleError::Signal { .. } => "Signal".to_string(),
        ModuleError::WrongType { expected, .. } => format!("WrongType/{expected:?}"),
        ModuleError::NonUnicodeString { .. } => "NonUnicodeString".to_string(),
        ModuleError::InvalidUtf8 { .. } => "InvalidUtf8".to_string(),
        ModuleError::BufferTooSmall { .. } => "BufferTooSmall".to_string(),
        ModuleError::IndexOutOfRange { .. } => "IndexOutOfRange".to_string(),
        ModuleError::IntegerOutOfRange { .. } => "IntegerOutOfRange".to_string(),
        _ => format!("Unknown: {error:?}"),
    }
}

fn describe_rust(error: &RustError) -> String {
    match error {
        RustError::WrongTypeUserPtr { .. } => "WrongTypeUserPtr".to_string(),
        RustError::InvalidUtf8 { .. } => "InvalidUtf8".to_string(),
        RustError::IntegerOutOfRange { .. } => "IntegerOutOfRange".to_string(),
        _ => format!("Unknown: {error:?}"),
    }
}

#[defun(mod_in_name = false)]
fn read_file<'e>(env: &Env, path: String) -> Result<String> {
    fs::read_to_string(path).or_signal(env, emrs_file_error)
}

#[defun(mod_in_name = false, name = "error:panic")]
fn panic(message: String) -> Result<()> {
    panic!("{}", message)
}

#[defun(mod_in_name = false, name = "error:signal")]
fn signal(env: &Env, symbol: Value, message: String) -> Result<()> {
    env.signal(symbol, (message,))
}

fn parse_arg(env: &CallEnv) -> Result<String> {
    let i: i64 = env.parse_arg(0)?;
    let s: String = env.parse_arg(i as usize)?;
    Ok(s)
}

emacs::define_errors! {
    emrs_file_error "File error"
    emacs_module_rs_test_error "Hello" (rust_error)
    error_defined_without_parent "Error"
}

pub fn init(env: &Env) -> Result<()> {
    emacs::__export_functions! {
        env, format!("{}error:", *MODULE_PREFIX), {
            "parse-arg"   => (parse_arg   , 2..5),
        }
    }

    #[defun(mod_in_name = false, name = "error:signal-custom")]
    fn signal_custom(env: &Env) -> Result<()> {
        env.signal(emacs_module_rs_test_error, [])
    }

    Ok(())
}
