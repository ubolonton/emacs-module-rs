use std;

use emacs::func::Manage;
use emacs::{Env, CallEnv, Value, IntoLisp, Result};

use super::MODULE_PREFIX;

fn test(env: &CallEnv) -> Result<Value> {
    5.into_lisp(env)?;
    match "1\0a".into_lisp(env) {
        Ok(_) => {
            println!("ok");
            call!(env, "message", "Should not get to this because we used a string with a zero byte")?;
        },
        Err(_) => {
            println!("err");
            call!(env, "message", "Caught error here and continue")?;
        }
    };

    println!("Start");
    let range = std::ops::Range { start: 0, end: 2usize.pow(22) };
    for i in range {
        println!("{}", i);
        let result = call!(env, "/", 1, 0);
        match result {
            _ => continue
        }
    }
    println!("Stop");

    let args = &[call!(env, "+", 1)?, "(+ 1) -> %s".into_lisp(env)?];
    env.call("message", args)?;

    // Wrong type argument: symbolp, (throw-)
    call!(env, "throw-", 1)?;

    call!(env, "error", 1)?;
    call!(env, "+", "1\0", 2)?;
    call!(env, "message", "Should not ever get here")
}

fn using_fset(env: &Env) -> Result<()> {
    make_prefix!(prefix, *MODULE_PREFIX);

    fn sum_and_diff(env: &CallEnv) -> Result<Value> {
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

fn using_defuns(env: &Env) -> Result<()> {
    struct StringWrapper {
        pub s: String
    }

    custom_types! {
        StringWrapper as "StrWrapper";
    }

    defuns! {
        env, *MODULE_PREFIX;

        inc, "1+", (env, x) {
            let i: i64 = x.into_rust()?;
            (i + 1).into_lisp(env)
        }

        identity, "not even doing any conversion", (_env, x) {
            Ok(x)
        }

        "to-uppercase", "", (env, s) {
            let s: String = s.into_rust()?;
            s.to_uppercase().into_lisp(env)
        }

        "make-dec", "", (env) {
            fn dec(env: &CallEnv) -> Result<Value> {
                let i: i64 = env.parse_arg(0)?;
                (i - 1).into_lisp(env)
            }
            emacs_lambda!(env, dec, 1..1, "decrement", std::ptr::null_mut())
        }

        "make-inc-and-plus", "", (env) {
            fn inc(env: &CallEnv) -> Result<Value> {
                let i: i64 = env.parse_arg(0)?;
                (i + 1).into_lisp(env)
            }

            fn plus(env: &CallEnv) -> Result<Value> {
                let x: i64 = env.parse_arg(0)?;
                let y: i64 = env.parse_arg(1)?;
                (x + y).into_lisp(env)
            }

            env.call("cons", &[
                emacs_lambda!(env, inc, 1..1, "increment")?,
                emacs_lambda!(env, plus, 2..2)?,
            ])
        }

        "wrap-string", "", (env, s) {
            let s: String = s.into_rust()?;
            let b = Box::new(StringWrapper { s });
            b.into_lisp(env)
        }
    }

    Ok(())
}

pub fn init(env: &Env) -> Result<()> {
    using_fset(env)?;
    using_defuns(env)?;

    fn sum(env: &CallEnv) -> Result<i64> {
        let x: i64 = env.parse_arg(0)?;
        let y: i64 = env.parse_arg(1)?;
        Ok(x + y)
    }

    emacs_export_functions! {
        env, *MODULE_PREFIX, {
            "test" => (test, 0..0, "doc string"),
            "sum" => (sum, 2..2),
        }
    }

    Ok(())
}
