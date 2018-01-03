extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate emacs;

#[macro_use]
mod macros;

use emacs::EmacsVal;
use emacs::{Env, ToEmacs, Result};
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

fn test(env: &mut Env, _args: &[EmacsVal], _data: *mut libc::c_void) -> Result<EmacsVal> {
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

fn init(env: &mut Env) -> Result<EmacsVal> {
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
            fn dec(env: &Env, args: &[EmacsVal], _data: *mut libc::c_void) -> Result<EmacsVal> {
                let i: i64 = env.from_emacs(args[0])?;
                (i - 1).to_emacs(env)
            }
            emacs_subrs! {
                dec -> f_dec;
            }
            env.make_function(f_dec, 1..1, "decrement", ptr::null_mut())
        }
    }

    env.provide(MODULE)
}
