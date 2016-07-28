pub mod emacs_module;

extern crate libc;
use emacs_module::{emacs_subr, EmacsEnv, EmacsRT, EmacsVal};
use std::ffi::CString;
use std::os::raw;


#[no_mangle]
pub extern "C" fn find_function(env: *mut EmacsEnv, name: String)
                                -> *mut EmacsVal {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern "C" fn make_function(env: *mut EmacsEnv,
                                min_args: i64,
                                max_args: i64,
                                f: emacs_subr,
                                doc: String,
                                user_ptr: *mut raw::c_void)
                                -> *mut EmacsVal {
    let doc = CString::new(doc).unwrap().as_ptr();
    unsafe {
        (*env).make_function.unwrap()(env, min_args, max_args, f, doc, user_ptr)
    }
}

pub extern "C" fn make_emacs_string<T: Into<Vec<u8>>>(env: *mut EmacsEnv,
                                                      string: T)
                                                      -> *mut EmacsVal {
    let c_string = CString::new(string).unwrap().as_ptr();
    let strlen = unsafe { libc::strlen(c_string) as i64 };
    unsafe { (*env).make_string.unwrap()(env, c_string, strlen) }
}

#[no_mangle]
pub extern "C" fn get_environment(ert: *mut EmacsRT) -> *mut EmacsEnv {
    unsafe { (*ert).get_environment.unwrap()(ert) }
}

#[no_mangle]
pub extern "C" fn intern_symbol(env: *mut EmacsEnv, name: String)
                                -> *mut EmacsVal {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern "C" fn bind_function(env: *mut EmacsEnv,
                                name: String,
                                sfun: *mut EmacsVal) {
    let qfset = find_function(env, "fset".to_string());
    let qsym = intern_symbol(env, name);
    let args = [qsym, sfun].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, qfset, 2, args) };
}

#[no_mangle]
pub extern "C" fn provide(env: *mut EmacsEnv, feature: String) {
    let feat = unsafe {
        (*env).intern.unwrap()(env, CString::new(feature).unwrap().as_ptr())
    };
    let provide = find_function(env, "provide".to_string());
    let args = [feat].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, provide, 1, args) };
}


#[no_mangle]
pub extern "C" fn get_buffer(env: *mut EmacsEnv, buffer: String)
                             -> *mut EmacsVal {
    let get_buffer = find_function(env, "get-buffer".to_string());
    let args = [make_emacs_string(env, buffer)].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, get_buffer, 1, args) }
}
