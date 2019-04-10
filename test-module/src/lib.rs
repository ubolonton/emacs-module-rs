use lazy_static::lazy_static;

use emacs;
use emacs::{CallEnv, emacs_lambda, Env, IntoLisp, Result, Value};

#[macro_use]
mod macros;

mod test_basics;
mod test_error;
mod test_lifetime;

mod ref_cell;
mod vector;
mod hash_map;

emacs::emacs_plugin_is_GPL_compatible!();

const MODULE: &str = "t";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

#[emacs::module(name(fn), separator = "/")]
fn t(env: &Env) -> Result<()> {
    env.message("Hello, Emacs!")?;

    test_basics::init(env)?;
    test_error::init(env)?;
    Ok(())
}

// -----------------------------------------------------------------------------
// Below are tests for functions declared at root of the crate. Don't move them elsewhere.

// Docstring above, with space.
/// 1+
#[emacs::func]
fn inc(x: i64) -> Result<i64> {
    Ok(x + 1)
}

// Docstring below, without space.
#[emacs::func]
///Return the input (not a copy).
fn identity(x: Value) -> Result<Value> {
    Ok(x)
}

#[emacs::func]
fn to_uppercase(s: String) -> Result<String> {
    Ok(s.to_uppercase())
}

struct StringWrapper {
    pub s: String
}

custom_types! {
    StringWrapper as "StrWrapper";
}

#[emacs::func]
fn wrap_string(s: String) -> Result<Box<StringWrapper>> {
    Ok(Box::new(StringWrapper { s }))
}

#[emacs::func]
fn make_dec(env: &Env) -> Result<Value<'_>> {
    fn dec(env: &CallEnv) -> Result<Value<'_>> {
        let i: i64 = env.parse_arg(0)?;
        (i - 1).into_lisp(env)
    }
    emacs_lambda!(env, dec, 1..1, "decrement", std::ptr::null_mut())
}

#[emacs::func]
fn make_inc_and_plus(env: &Env) -> Result<Value<'_>> {
    fn inc(env: &CallEnv) -> Result<Value<'_>> {
        let i: i64 = env.parse_arg(0)?;
        (i + 1).into_lisp(env)
    }

    fn plus(env: &CallEnv) -> Result<Value<'_>> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        (x + y).into_lisp(env)
    }

    env.call("cons", &[
        emacs_lambda!(env, inc, 1..1, "increment")?,
        emacs_lambda!(env, plus, 2..2)?,
    ])
}
