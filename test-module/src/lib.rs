extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate emacs;

#[macro_use]
mod macros;

use emacs::{Env, Value, ToLisp, IntoLisp, Result};
use emacs::HandleFunc;
use std::ptr;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

const MODULE: &str = "t";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

fn test<'e>(env: &'e Env, _args: &[Value<'e>], _data: *mut libc::c_void) -> Result<Value<'e>> {
    env.clone_to_lisp(5)?;
    match "1\0a".to_lisp(env) {
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

    let args = &[call!(env, "+", 1)?, "(+ 1) -> %s".to_lisp(env)?];
    env.call("message", args)?;

    // Wrong type argument: symbolp, (throw-)
    call!(env, "throw-", 1)?;

    call!(env, "error", 1)?;
    call!(env, "+", "1\0", 2)?;
    call!(env, "message", "Should not ever get here")
}

fn init_vector_functions(env: &Env) -> Result<()> {
    make_prefix!(prefix, *MODULE_PREFIX);

    struct Vector {
        pub x: i64,
        pub y: i64,
    }

    custom_types! {
        Vector as "Vector";
    }

    fn swap_components<'e>(env: &'e Env, args: &[Value<'e>], _data: *mut libc::c_void) -> Result<Value<'e>> {
        let mut v = args[0];
        {
            let vec: &mut Vector = unsafe { v.to_mut(env)? };
            vec.x = vec.x ^ vec.y;
            vec.y = vec.x ^ vec.y;
            vec.x = vec.x ^ vec.y;
        }
        Ok(v)
    }

    emacs_subrs! {
        swap_components -> f_swap_components;
    }

    env.register(
        prefix!("vector:swap-components"), f_swap_components, 1..1,
        "", ptr::null_mut()
    )?;

    defuns! {
        env, format!("{}vector:", *MODULE_PREFIX);

        "make", "", (env, x, y) {
            let x: i64 = x.to_owned(env)?;
            let y: i64 = env.get_owned(y)?;
            let b = Box::new(Vector { x, y });
            env.move_to_lisp(b)
        }

        "to-list", "", (env, v) {
            env.get_ref::<Vector>(&v)?;
            let v: &Vector = env.get_ref(&v)?;
            let x = v.x.to_lisp(env)?;
            let y = v.y.to_lisp(env)?;
            env.list(&[x, y])
        }

        "add", "", (env, a, b) {
            let a: &Vector = a.to_ref(env)?;
            let b: &Vector = b.to_ref(env)?;
            let (x, y) = (b.x + a.x, b.y + a.y);
            Box::new(Vector { x, y }).into_lisp(env)
        }

        "scale-mutably", "", (env, times, v) {
            let times: i64 = times.to_owned(env)?;
            {
                let mut v = v;
                let v = unsafe { v.to_mut::<Vector>(env)? };
                v.x *= times;
                v.y *= times;
            }
            env.intern("nil")
        }
    }
    Ok(())
}

fn init(env: &Env) -> Result<Value> {
    make_prefix!(prefix, *MODULE_PREFIX);

    env.message("Hello, Emacs!")?;

    emacs_subrs! {
        test -> f_test;
    }

    env.register(
        prefix!(test), f_test, 0..0,
        "", ptr::null_mut()
    )?;

    init_vector_functions(env)?;

    struct StringWrapper {
        pub s: String
    }

    custom_types! {
        StringWrapper as "StrWrapper";
    }

    defuns! {
        env, *MODULE_PREFIX;

        inc, "1+", (env, x) {
            let i: i64 = x.to_owned(env)?;
            (i + 1).to_lisp(env)
        }

        identity, "not even doing any conversion", (_env, x) {
            Ok(x)
        }

        "to-uppercase", "", (env, s) {
            let s: String = s.to_owned(env)?;
            s.to_uppercase().to_lisp(env)
        }

        "calling-error", "", (env) {
            call!(env, "/", 1, 0)
        }

        "make-dec", "", (env) {
            fn dec<'e>(env: &'e Env, args: &[Value<'e>], _data: *mut libc::c_void) -> Result<Value<'e>> {
                let i: i64 = args[0].to_owned(env)?;
                (i - 1).to_lisp(env)
            }
            emacs_subrs! {
                dec -> f_dec;
            }
            env.make_function(f_dec, 1..1, "decrement", ptr::null_mut())
        }

        "wrap-string", "", (env, s) {
            let s: String = s.to_owned(env)?;
            let b = Box::new(StringWrapper { s });
            env.move_to_lisp(b)
        }
    }

    env.provide(MODULE)
}
