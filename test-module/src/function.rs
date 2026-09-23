//! Testing function definition and calling.

use emacs::{defun, CallEnv, Env, IntoLisp, Result, Value};
use emacs::func::Manage;

use super::MODULE_PREFIX;

fn using_fset(env: &Env) -> Result<()> {
    make_prefix!(prefix, *MODULE_PREFIX);

    fn sum_and_diff(env: &CallEnv) -> Result<Value<'_>> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        env.list(&[(x + y).into_lisp(env)?, (x - y).into_lisp(env)?])
    }

    env.fset(prefix!("sum-and-diff"), emacs::lambda!(env, sum_and_diff, 2..2)?)?;

    Ok(())
}

pub fn init(env: &Env) -> Result<()> {
    using_fset(env)?;

    fn sum(env: &CallEnv) -> Result<i64> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        Ok(x + y)
    }

    emacs::__export_functions! {
        env, *MODULE_PREFIX, {
            "sum" => (sum, 2..2),
        }
    }

    Ok(())
}

// Test that raw identifiers are handled correctly. Note that it must be a reserved keyword,
// otherwise syn parses it into a non-raw identifier.
#[defun]
fn r#match() -> Result<()> {
    Ok(())
}

#[defun]
fn ignore_args(_: &Env, _: u8, _: u16) -> Result<()> {
    Ok(())
}

#[defun]
fn call_list(env: &Env, n: u16) -> Result<Value> {
    let x = "x";
    let y = 1;
    let z = true;

    let vx = x.into_lisp(env)?;
    let vy = y.into_lisp(env)?;
    let vz = z.into_lisp(env)?;

    let list = env.intern("list")?;
    let list_subr = env.call("symbol-function", [list])?;

    // Passing statically-sized slices and arrays.
    env.call("list", &[])?;
    env.call("list", [])?;
    env.call("list", &[vx, vy, vz])?;
    env.call("list", [vx, vy, vz])?;
    env.call("list", [vx])?;

    // Passing tuples.
    env.call("list", (x, y, z))?;

    // Calling by symbol, subr, owned string.
    env.call(list, [])?;
    env.call(list_subr, (x, y, z))?;
    env.call(String::from("list"), [])?;

    // Passing dynamically-sized slice.
    let mut ints = vec![];
    for i in 0..n {
        ints.push(i.into_lisp(env)?);
    }
    env.call(list, ints.as_slice())
}

#[defun]
fn call_value<'e>(function: Value<'e>, arg: Value) -> Result<Value<'e>> {
    function.call([arg])
}

#[defun]
fn call_mapc_vec(function: Value, vector: emacs::Vector) -> Result<()> {
    for (i, elem) in vector.into_iter().enumerate() {
        function.call((i, elem))?;
    }
    Ok(())
}
