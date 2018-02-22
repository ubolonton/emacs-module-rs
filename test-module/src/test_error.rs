use emacs::{Env, CallEnv, Value, IntoLisp, Result};

use super::MODULE_PREFIX;

fn lisp_divide(env: &CallEnv) -> Result<i64> {
    let x: i64 = env.parse_arg(0)?;
    let y: i64 = env.parse_arg(1)?;

    fn inner(env: &Env, x: i64, y: i64) -> Result<Value> {
        call!(env, "/", x, y)
    }

    inner(env, x, y)?.into_rust()
}

pub fn init(env: &Env) -> Result<()> {
    emacs_export_functions! {
        env, format!("{}error:", *MODULE_PREFIX), {
            "lisp-divide" => (lisp_divide, 2..2)
        }
    }

    Ok(())
}
