extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate emacs;

#[macro_use]
mod macros;

use std::ptr;
use std::cell::RefCell;
use emacs::{Env, CallEnv, Value, IntoLisp, Result, Error};
use emacs::func::Manage;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

const MODULE: &str = "t";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

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

fn init_vector_functions(env: &Env) -> Result<()> {
    struct Vector {
        pub x: i64,
        pub y: i64,
    }

    custom_types! {
        Vector as "Vector";
    }

    fn swap_components(env: &CallEnv) -> Result<Value> {
        let mut v = env.get_arg(0).clone();
        {
            let vec: &mut Vector = unsafe { v.get_mut()? };
            vec.x = vec.x ^ vec.y;
            vec.y = vec.x ^ vec.y;
            vec.x = vec.x ^ vec.y;
        }
        Ok(v)
    }

    emacs_export_functions! {
        env, format!("{}vector:", *MODULE_PREFIX), {
            "swap-components" => (swap_components, 1..1)
        }
    }

    defuns! {
        env, format!("{}vector:", *MODULE_PREFIX);

        "make", "", (env, x, y) {
            let x: i64 = x.into_rust()?;
            let y: i64 = y.into_rust()?;
            let b = Box::new(Vector { x, y });
            b.into_lisp(env)
        }

        "to-list", "", (env, v) {
            v.get_ref::<Vector>()?;
            let v: &Vector = v.get_ref()?;
            let x = v.x.into_lisp(env)?;
            let y = v.y.into_lisp(env)?;
            env.list(&[x, y])
        }

        "add", "", (env, a, b) {
            let a: &Vector = a.get_ref()?;
            let b: &Vector = b.get_ref()?;
            let (x, y) = (b.x + a.x, b.y + a.y);
            Box::new(Vector { x, y }).into_lisp(env)
        }

        "scale-mutably", "", (env, times, v) {
            let times: i64 = times.into_rust()?;
            {
                let mut v = v;
                let v = unsafe { v.get_mut::<Vector>()? };
                v.x *= times;
                v.y *= times;
            }
            env.intern("nil")
        }
    }
    Ok(())
}

// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.
fn init_test_ref_cell(env: &Env) -> Result<()> {
    fn make(env: &CallEnv) -> Result<RefCell<i64>> {
        let x: i64 = env.parse_arg(0)?;
        Ok(RefCell::new(x))
    }

    fn mutate_twice(env: &CallEnv) -> Result<()> {
        let r = env.get_arg(0);
        let r: &RefCell<i64> = r.get_ref()?;
        let mut x = r.try_borrow_mut().map_err(Error::new)?;
        let mut y = r.try_borrow_mut().map_err(Error::new)?;
        *x = 1;
        *y = 2;
        Ok(())
    }

    emacs_export_functions! {
        env, format!("{}refcell:", *MODULE_PREFIX), {
            "make"         => (make, 1..1, "Wrap the given integer in a RefCell."),
            "mutate-twice" => (mutate_twice, 1..1, "This should fail at run time due to double mut borrows."),
        }
    }

    Ok(())
}

fn init_test_simplified_fns(env: &Env) -> Result<()> {
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

fn init(env: &Env) -> Result<Value> {
    env.message("Hello, Emacs!")?;

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

    init_vector_functions(env)?;
    init_test_ref_cell(env)?;
    init_test_simplified_fns(env)?;

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

        "calling-error", "", (env) {
            call!(env, "/", 1, 0)
        }

        "make-dec", "", (env) {
            fn dec(env: &CallEnv) -> Result<Value> {
                let i: i64 = env.parse_arg(0)?;
                (i - 1).into_lisp(env)
            }
            emacs_lambda!(env, dec, 1..1, "decrement", ptr::null_mut())
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

    env.provide(MODULE)
}
