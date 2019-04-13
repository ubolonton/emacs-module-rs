use emacs::func;
use emacs::{CallEnv, Env, Result, Value};
use emacs::ErrorKind::{self, Signal, Throw};

use super::MODULE_PREFIX;

#[func(mod_in_name = false, name = "error:lisp-divide")]
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

#[func(mod_in_name = false, name = "error:get-type")]
fn get_type(f: Value<'_>) -> Result<Value<'_>> {
    let env = f.env;
    match env.call("funcall", &[f]) {
        Err(error) => {
            if let Some(&Signal { ref symbol, .. }) = error.downcast_ref::<ErrorKind>() {
                unsafe {
                    return Ok(symbol.value(env));
                }
            }
            Err(error)
        }
        v => v,
    }
}

#[func(mod_in_name = false, name = "error:catch")]
fn catch<'e>(expected_tag: Value<'e>, f: Value<'e>) -> Result<Value<'e>> {
    let env = expected_tag.env;
    match env.call("funcall", &[f]) {
        Err(error) => {
            if let Some(&Throw { ref tag, ref value }) = error.downcast_ref::<ErrorKind>() {
                unsafe {
                    if env.eq(tag.value(env), expected_tag) {
                        return Ok(value.value(env));
                    }
                }
            }
            Err(error)
        }
        v => v,
    }
}

fn parse_arg(env: &CallEnv) -> Result<String> {
    let i: i64 = env.parse_arg(0)?;
    let s: String = env.parse_arg(i as usize)?;
    Ok(s)
}

pub fn init(env: &Env) -> Result<()> {
    emacs::export_functions! {
        env, format!("{}error:", *MODULE_PREFIX), {
            "parse-arg"   => (parse_arg   , 2..5),
        }
    }

    Ok(())
}
