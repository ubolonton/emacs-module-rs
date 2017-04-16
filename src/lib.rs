#![feature(untagged_unions)]
extern crate libc;

mod emacs_gen;
pub mod hlapi;

pub use emacs_gen::{Dtor, EmacsEnv, EmacsRT, EmacsVal, EmacsSubr};
pub use hlapi::{destruct, eq, register, ConvErr, ConvResult};
pub use hlapi::elisp2native as elisp2native;
pub use hlapi::native2elisp as native2elisp;
use std::ffi::CString;

#[no_mangle]
pub extern "C" fn find_function(env: *mut EmacsEnv, name: &str) -> EmacsVal {
    unsafe {
        let intern = (*env).intern.unwrap();
        intern(env, CString::new(name).unwrap().as_ptr())
    }
}

#[no_mangle]
pub extern "C" fn make_function(env: *mut EmacsEnv,
                                min_args: i64,
                                max_args: i64,
                                f: Option<EmacsSubr>,
                                doc: &str,
                                user_ptr: *mut libc::c_void) -> EmacsVal {
    let doc = CString::new(doc).unwrap().as_ptr();
    unsafe {
        let make_function = (*env).make_function.unwrap();
        make_function(env, min_args as isize, max_args as isize, f, doc,
                      user_ptr as *mut std::os::raw::c_void)
    }
}

pub extern "C" fn make_emacs_string<S>(env: *mut EmacsEnv, string: S)
                                       -> EmacsVal where S: Into<Vec<u8>> {
    let c_string = CString::new(string).unwrap().as_ptr();
    unsafe {
        let strlen = libc::strlen(c_string) as isize;
        let make_string = (*env).make_string.unwrap();
        make_string(env, c_string, strlen)
    }
}

#[no_mangle]
pub extern "C" fn get_environment(ert: *mut EmacsRT) -> *mut EmacsEnv {
    unsafe {
        let get_env = (*ert).get_environment.unwrap();
        get_env(ert)
    }
}

#[no_mangle]
pub extern "C" fn intern_symbol(env: *mut EmacsEnv, name: String) -> EmacsVal {
    unsafe {
        let intern = (*env).intern.unwrap();
        intern(env, CString::new(name).unwrap().as_ptr())
    }
}

#[no_mangle]
pub extern "C" fn bind_function(env: *mut EmacsEnv,
                                name: String,
                                sfun: EmacsVal) {
    let qfset = find_function(env, "fset");
    let qsym = intern_symbol(env, name);
    let args = [qsym, sfun].as_mut_ptr();
    unsafe {
        let funcall = (*env).funcall.unwrap();
        funcall(env, qfset, 2, args)
    };
}

#[no_mangle]
pub extern "C" fn provide(env: *mut EmacsEnv, feature: String) {
    let feat = unsafe {
        let intern = (*env).intern.unwrap();
        intern (env, CString::new(feature).unwrap().as_ptr())
    };
    let provide = find_function(env, "provide");
    let args = [feat].as_mut_ptr();
    unsafe {
        let funcall = (*env).funcall.unwrap();
        funcall(env, provide, 1, args)
    };
}


#[no_mangle]
pub extern "C" fn get_buffer(env: *mut EmacsEnv, buffer: String) -> EmacsVal {
    let get_buffer = find_function(env, "get-buffer");
    let args = [make_emacs_string(env, buffer)].as_mut_ptr();
    unsafe {
        let funcall = (*env).funcall.unwrap();
        funcall(env, get_buffer, 1, args)
    }
}

#[no_mangle]
pub extern "C" fn call(env: *mut EmacsEnv, fn_name: &str, args: &mut [EmacsVal])
                       -> EmacsVal {
    let callee = find_function(env,  fn_name);
    unsafe {
        let funcall = (*env).funcall.unwrap();
        funcall(env, callee, args.len() as isize, args.as_mut_ptr())
    }
}
