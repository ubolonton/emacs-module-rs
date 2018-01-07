extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate emacs;

#[macro_use]
mod macros;

use emacs::{Env, Value, ToEmacs, Result};
use emacs::HandleFunc;
use std::ptr;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

const MODULE: &str = "test-module";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

macro_rules! call {
    ($env:ident, $name:expr $(, $arg:expr)*) => {{
        let args = &mut [$($arg.to_emacs($env)?,)*];
        $env.call($name, args)
    }}
}

fn test(env: &mut Env, _args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
    env.to_emacs(5)?;
    match "1\0a".to_emacs(env) {
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

    let args = &mut [call!(env, "+", 1)?, "(+ 1) -> %s".to_emacs(env)?];
    env.call("message", args)?;

    // Wrong type argument: symbolp, (throw-)
    call!(env, "throw-", 1)?;

    call!(env, "error", 1)?;
    call!(env, "+", "1\0", 2)?;
    call!(env, "message", "Should not ever get here")
}

emacs_subrs! {
    test -> f_test;
}

struct Vector {
    pub x: i64,
    pub y: i64,
}

struct StringWrapper {
    pub s: String
}

custom_types! {
    Vector as "Vector";
    StringWrapper as "StrWrapper";
}

fn init(env: &mut Env) -> Result<Value> {
    make_prefix!(prefix, *MODULE_PREFIX);

    env.message("Hello, Emacs!")?;

    env.register(
        prefix!(test), f_test, 0..0,
        "", ptr::null_mut()
    )?;

    defuns! {
        env, *MODULE_PREFIX;

        inc, "1+", (env, x) {
            let i: i64 = env.from_emacs(x)?;
            (i + 1).to_emacs(env)
        }

        identity, "not even doing any conversion", (_env, x) {
            Ok(x)
        }

        "to-uppercase", "", (env, s) {
            let s: String = env.from_emacs(s)?;
            s.to_uppercase().to_emacs(env)
        }

        "calling-error", "", (env) {
            call!(env, "/", 1, 0)
        }

        "make-dec", "", (env) {
            fn dec(env: &Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
                let i: i64 = env.from_emacs(&args[0])?;
                (i - 1).to_emacs(env)
            }
            emacs_subrs! {
                dec -> f_dec;
            }
            env.make_function(f_dec, 1..1, "decrement", ptr::null_mut())
        }

        "make-vector", "", (env, x, y) {
            let x: i64 = env.from_emacs(x)?;
            let y: i64 = env.from_emacs(y)?;
            let b = Box::new(Vector { x, y });
            env.take(b)
        }

        "wrap-string", "", (env, s) {
            let s: String = env.from_emacs(s)?;
            let b = Box::new(StringWrapper { s });
            env.take(b)
        }

        "vector-to-list", "", (env, v) {
            env.try_ref::<Vector>(&v)?;
            let v: &Vector = env.try_ref(&v)?;
            let x = v.x.to_emacs(env)?;
            let y = v.y.to_emacs(env)?;
            env.list(&mut [x, y])
        }

        "add-vectors", "", (env, a, b) {
            let a: &Vector = env.try_ref(&a)?;
            let b: &Vector = b.try_borrow(env)?;
            let (x, y) = (b.x + a.x, b.y + a.y);
            env.take(Box::new(Vector { x, y }))
        }

        "scale-vector-mutably", "", (env, times, v) {
            let times: i64 = env.from_emacs(&times)?;
            {
                let v: &mut Vector = env.try_mut(v)?;
                v.x *= times;
                v.y *= times;
            }
            env.intern("nil")
        }
    }

    env.provide(MODULE)
}
