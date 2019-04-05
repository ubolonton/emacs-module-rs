use emacs::{emacs_lambda, emacs_export_functions};
use emacs::func::Manage;
use emacs::{Env, CallEnv, Value, IntoLisp, Result};

use super::MODULE_PREFIX;

fn using_fset(env: &Env) -> Result<()> {
    make_prefix!(prefix, *MODULE_PREFIX);

    fn sum_and_diff(env: &CallEnv) -> Result<Value<'_>> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        env.list(&[
            (x + y).into_lisp(env)?,
            (x - y).into_lisp(env)?
        ])
    }

    env.fset(
        prefix!("sum-and-diff"),
        emacs_lambda!(env, sum_and_diff, 2..2)?
    )?;

    Ok(())
}

fn to_lowercase_or_nil(env: &CallEnv) -> Result<Value<'_>> {
    let input: Option<String> = env.parse_arg(0)?;
    let output = input.map(|s| s.to_lowercase());
    // This tests IntoLisp for Option<&str>. It looks a bit convoluted. TODO: Improve it.
    let r: Option<&str> = match &output {
        &None => None,
        &Some(ref s) => Some(s),
    };
    r.into_lisp(env)
}

pub fn init(env: &Env) -> Result<()> {
    using_fset(env)?;

    fn sum(env: &CallEnv) -> Result<i64> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        Ok(x + y)
    }

    emacs_export_functions! {
        env, *MODULE_PREFIX, {
            "sum" => (sum, 2..2),
            "to-lowercase-or-nil" => (to_lowercase_or_nil, 1..1),
        }
    }

    Ok(())
}
