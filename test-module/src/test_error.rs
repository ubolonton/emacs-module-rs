//! Testing error reporting and handling.

use std::fs;

use emacs::{defun, CallEnv, Env, Result, Value};
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
                    if tag.value(env).eq(expected_tag) {
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

#[defun(mod_in_name = false)]
fn read_file<'e>(env: &Env, path: String) -> Result<String> {
    fs::read_to_string(path).or_signal(env, emrs_file_error)
}

#[defun(mod_in_name = false, name = "error:panic")]
fn panic(message: String) -> Result<()> {
    panic!(message)
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
