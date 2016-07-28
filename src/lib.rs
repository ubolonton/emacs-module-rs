extern crate libc;

pub mod emacs_module;

use emacs_module::{emacs_subr, EmacsEnv, EmacsRT, EmacsVal};
use std::ffi::CString;

#[no_mangle]
pub extern "C" fn find_function(env: *mut EmacsEnv, name: &str)
                                -> *mut EmacsVal {
    unsafe {
        let intern = (*env).intern.unwrap();
        intern(env, CString::new(name).unwrap().as_ptr())
    }
}

#[no_mangle]
pub extern "C" fn make_function(env: *mut EmacsEnv,
                                min_args: i64,
                                max_args: i64,
                                f: emacs_subr,
                                doc: &str,
                                user_ptr: *mut libc::c_void)
                                -> *mut EmacsVal {
    let doc = CString::new(doc).unwrap().as_ptr();
    unsafe {
        let make_function = (*env).make_function.unwrap();
        make_function(env, min_args, max_args, f, doc, user_ptr)
    }
}

pub extern "C" fn make_emacs_string<S>(env: *mut EmacsEnv,
                                       string: S)
                                       -> *mut EmacsVal where S: Into<Vec<u8>> {
    let c_string = CString::new(string).unwrap().as_ptr();
    unsafe {
        let strlen = libc::strlen(c_string) as i64;
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
pub extern "C" fn intern_symbol(env: *mut EmacsEnv, name: String)
                                -> *mut EmacsVal {
    unsafe {
        let intern = (*env).intern.unwrap();
        intern (env, CString::new(name).unwrap().as_ptr())
    }
}

#[no_mangle]
pub extern "C" fn bind_function(env: *mut EmacsEnv,
                                name: String,
                                sfun: *mut EmacsVal) {
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
pub extern "C" fn get_buffer(env: *mut EmacsEnv, buffer: String)
                             -> *mut EmacsVal {
    let get_buffer = find_function(env, "get-buffer");
    let args = [make_emacs_string(env, buffer)].as_mut_ptr();
    unsafe {
        let funcall = (*env).funcall.unwrap();
        funcall(env, get_buffer, 1, args)
    }
}
