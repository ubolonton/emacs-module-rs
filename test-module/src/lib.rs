extern crate libc;
#[macro_use]
extern crate emacs_module_bindings as emacs;

use emacs::{EmacsVal, EmacsRT, EmacsEnv, ConvResult};
use emacs::native2elisp as n2e;
use emacs::elisp2native as e2n;
use emacs::new::{Env, ToEmacs};
use emacs::error::{Result, Error};
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

/// Entry point for live-reloading during development.
#[no_mangle]
pub extern "C" fn emacs_rs_module_init(env: *mut EmacsEnv) -> libc::c_int {
    message!(env, "Hello, Emacs!").unwrap();

    let doc = CString::new("This is a unicode doc string, from Nguyễn Tuấn Anh!").unwrap();
    emacs::bind_function(
        env, format!("{}/inc", MODULE),
        n2e::function(
            env, 1, 1, Some(f_inc),
            doc.as_ptr(), ptr::null_mut(),
        ).unwrap(),
    );

    emacs::bind_function(
        env, format!("{}/call", MODULE),
        n2e::function(
            env, 0, 0, Some(f_call),
            CString::new("").unwrap().as_ptr(), ptr::null_mut(),
        ).unwrap(),
    );

    emacs::bind_function(
        env, format!("{}/test", MODULE),
        n2e::function(
            env, 0, 0, Some(f_test),
            CString::new("").unwrap().as_ptr(), ptr::null_mut(),
        ).unwrap(),
    );

    emacs::provide(env, MODULE.to_string());
    0
}

/// Entry point for Emacs' loader, for "production".
#[no_mangle]
pub extern "C" fn emacs_module_init(ert: *mut EmacsRT) -> libc::c_int {
//    let env = emacs::get_environment(ert);
//    emacs_rs_module_init(env)
    emacs_rs_module_init(Env::from(ert).raw())
}
