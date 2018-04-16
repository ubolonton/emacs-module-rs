use std;

use emacs::func::Manage;
use emacs::{Env, CallEnv, Value, IntoLisp, Result};

use super::MODULE_PREFIX;

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

fn to_lowercase_or_nil(env: &CallEnv) -> Result<Value> {
    let input: Option<String> = env.parse_arg(0)?;
    let output = input.map(|s| s.to_lowercase());
    // This tests IntoLisp for Option<&str>. It looks a bit convoluted. TODO: Improve it.
    let r: Option<&str> = match &output {
        &None => None,
        &Some(ref s) => Some(s),
    };
    r.into_lisp(env)
}

fn _use_after_gc(env: &CallEnv) -> Result<Value> {
    let n = 2;
    let h = env.call("make-hash-table", &[
        env.intern(":test")?,
        env.intern("equal")?,
        env.intern(":size")?,
        n.into_lisp(env)?
    ])?;
    let mut v = Vec::with_capacity(n as usize);
    for _ in 0..n {
        v.push("0123456789".into_lisp(env)?);
        env.call("garbage-collect", &[])?;
    }
    env.call("puthash", &[
        "0".into_lisp(env)?,
        env.call("list", &v)?,
        h,
    ])?;

    let l = env.call("list", &[h])?;

    // for _ in 0..5 {
    //     env.call("garbage-collect", &[])?;
    // }

    env.call("print", &[l])?;
    Ok(l)
}

fn use_after_gc(env: &CallEnv) -> Result<Value> {
    let n = 2;
    let mut v = Vec::with_capacity(n as usize);

    // // Crashes
    // for _ in 0..n {
    //     v.push("0".into_lisp(env)?);
    //     env.call("garbage-collect", &[])?;
    // }

    // // Crashes
    // let mut i = 0;
    // while i < n {
    //     i = i + 1;
    //     v.push("0".into_lisp(env)?);
    //     env.call("garbage-collect", &[])?;
    // }

    // // Crashes
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // env.call("garbage-collect", &[])?;

    // // Doesn't crash
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // env.call("garbage-collect", &[]);

    // // Doesn't crashes
    // v.push("0".into_lisp(env).unwrap());
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // env.call("garbage-collect", &[]);

    // // Crashes
    // v.push("0".into_lisp(env).unwrap());
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // v.push("0".into_lisp(env)?);
    // env.call("garbage-collect", &[])?;

    // Crashes
    (|| {
        v.push("0".into_lisp(env).unwrap());
    })();
    "0".into_lisp(env)?;
    // env.message("0")?; // Also crashes
    env.call("garbage-collect", &[])?;

    // // Doesn't crash (? -> .unwrap())
    // (|| {
    //     v.push("0".into_lisp(env).unwrap());
    // })();
    // "0".into_lisp(env).unwrap();
    // env.call("garbage-collect", &[])?;

    // // Doesn't crash
    // {
    //     v.push("0".into_lisp(env)?);
    //     env.call("garbage-collect", &[])?;
    // }
    // {
    //     v.push("0".into_lisp(env)?);
    //     env.call("garbage-collect", &[])?;
    // }

    // // Doesn't crash
    // let x = "0".into_lisp(env)?;
    // for _ in 0..n {
    //     v.push(x);
    //     env.call("garbage-collect", &[])?;
    // }

    println!("v={:?}", v);
    let l = env.call("list", &v)?;
    println!("1");
    env.call("print", &[l])?;
    println!("2");
    Ok(l)
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
            "sum" => (sum, 2..2),
            "to-lowercase-or-nil" => (to_lowercase_or_nil, 1..1),
            "use-after-gc" => (use_after_gc, 0..0),
        }
    }

    Ok(())
}
