extern crate libc;
#[macro_use]
extern crate emacs_module_bindings as emacs;

use emacs::{EmacsVal, EmacsRT, EmacsEnv, ConvResult};
use emacs::native2elisp as n2e;
use emacs::elisp2native as e2n;
use emacs::new::{Env, ToEmacs};
use emacs::error::{Result, Error};
use emacs::func::Func;
use std::os::raw;
use std::ptr;
use std::ffi::CString;

/// This states that the module is GPL-compliant.
/// Emacs won't load the module if this symbol is undefined.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

const MODULE: &str = "test-module";

fn inc(env: *mut EmacsEnv, num: *mut EmacsVal) -> ConvResult<EmacsVal> {
    let i = e2n::integer(env, num, 0)?;
    n2e::integer(env, i + 1)
}

fn call(raw: *mut EmacsEnv) -> ConvResult<EmacsVal> {
    let env = &Env::from(raw);
    env.call("message", &mut [
        "Testing %s".to_emacs(env)?,
        "formatting".to_emacs(env)?,
    ])?;
    env.list(&mut [
        1.to_emacs(env)?,
        2.to_emacs(env)?,
    ])?;
    env.call("xxx", &mut [])?;
    env.call("+", &mut [
        "1\0".to_emacs(env)?,
        2.to_emacs(env)?,
    ])?;
    message!(raw, "Here")
}

fn test(env: &Env, _args: &mut [EmacsVal], _data: *mut raw::c_void) -> Result<EmacsVal> {
//    env.call("message", &mut [
//        "Testing %s".to_emacs(env)?,
//        "arithmetic".to_emacs(env)?,
//    ])?;
//    env.list(&mut [
//        1.to_emacs(env)?,
//        2.to_emacs(env)?,
//    ])?;
//    Err(Error::signal(
//        env.intern("error")?,
//        env.list(&mut [
//            "Custom error signaled from Rust".to_emacs(env)?
//        ])?
//    ))
    env.to_emacs(&5)?;
    match "1\0a".to_emacs(env) {
        Ok(_) => {
            println!("ok");
            env.call("message", &mut [
                "Should not get to this because we used a string with a zero byte".to_emacs(env)?
            ])?;
        },
        Err(_) => {
            println!("err");
            env.call("message", &mut [
                "Caught error here and continue".to_emacs(env)?
            ])?;
        }
    };
    env.call("+", &mut [
        "1\0".to_emacs(env)?,
        2.to_emacs(env)?,
    ])?;
    env.call("message", &mut [
        "Should not ever get here".to_emacs(env)?
    ])
}

emacs_subrs!(
    f_inc(env, _nargs, args, _data, tag) {
        message!(env, "{}: {:?}", tag, args)?;
        inc(env, args)
    };

    f_call(env, _nargs, args, _data, _tag) {
        call(env)
    };
);

expose_subrs!(
    test -> f_test;
);

fn init(env: &Env) -> Result<EmacsVal> {
    env.message("Hello, Emacs!")?;

    let doc = env.to_cstring("This is a unicode doc string, from Nguyễn Tuấn Anh!")?;
    env.fset(
        &format!("{}/inc", MODULE),
        env.make_function(1, 1, f_inc, doc.as_ptr(), ptr::null_mut())?
    )?;

    env.fset(
        &format!("{}/call", MODULE),
        env.make_function(0, 0, f_call,
        env.to_cstring("")?.as_ptr(), ptr::null_mut())?
    )?;

    env.fset(
        &format!("{}/test", MODULE),
        env.make_function(0, 0, f_test,
        env.to_cstring("")?.as_ptr(), ptr::null_mut())?
    )?;

    env.call(&format!("{}/call", MODULE), &mut [])?;

    env.provide(MODULE)
}

/// Entry point for live-reloading during development.
#[no_mangle]
pub extern "C" fn emacs_rs_module_init(raw: *mut EmacsEnv) -> libc::c_int {
    match init(&Env::from(raw)) {
        Ok(_) => 0,
        Err(_) => 1,
    }
}

/// Entry point for Emacs' loader, for "production".
#[no_mangle]
pub extern "C" fn emacs_module_init(ert: *mut EmacsRT) -> libc::c_int {
    emacs_rs_module_init(Env::from(ert).raw())
}
