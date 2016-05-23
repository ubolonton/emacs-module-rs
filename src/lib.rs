pub mod emacs_module;

extern crate libc;
use emacs_module::{emacs_subr, emacs_env, emacs_value, Struct_emacs_value_tag,
                   Struct_emacs_runtime, Struct_emacs_env_25};
use std::ffi::CString;
use std::os::raw;


#[no_mangle]
pub extern "C" fn find_function(env: *mut emacs_env, name: String) -> *mut Struct_emacs_value_tag {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern "C" fn make_function(env: *mut emacs_env,
                                min_args: i64,
                                max_args: i64,
                                f: emacs_subr,
                                doc: String,
                                user_ptr: *mut raw::c_void)
                                -> *mut Struct_emacs_value_tag {
    let doc = CString::new(doc).unwrap().as_ptr();
    unsafe { (*env).make_function.unwrap()(env, min_args, max_args, f, doc, user_ptr) }
}

pub extern "C" fn make_emacs_string<T: Into<Vec<u8>>>(env: *mut emacs_env,
                                                      string: T)
                                                      -> *mut Struct_emacs_value_tag {
    let c_string = CString::new(string).unwrap().as_ptr();
    let strlen = unsafe { libc::strlen(c_string) as i64 };
    unsafe { (*env).make_string.unwrap()(env, c_string, strlen) }
}

#[no_mangle]
pub extern "C" fn get_environment(ert: *mut Struct_emacs_runtime) -> *mut Struct_emacs_env_25 {
    unsafe { (*ert).get_environment.unwrap()(ert) }
}

#[no_mangle]
pub extern "C" fn intern_symbol(env: *mut emacs_env, name: String) -> *mut Struct_emacs_value_tag {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern "C" fn bind_function(env: *mut emacs_env, name: String, sfun: emacs_value) {
    let qfset = find_function(env, "fset".to_string());
    let qsym = intern_symbol(env, name);
    let args = [qsym, sfun].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, qfset, 2, args) };
}

#[no_mangle]
pub extern "C" fn provide(env: *mut emacs_env, feature: String) {
    let feat = unsafe { (*env).intern.unwrap()(env, CString::new(feature).unwrap().as_ptr()) };
    let provide = find_function(env, "provide".to_string());
    let args = [feat].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, provide, 1, args) };
}


#[no_mangle]
pub extern "C" fn get_buffer(env: *mut emacs_env, buffer: String) -> *mut Struct_emacs_value_tag {
    let get_buffer = find_function(env, "get-buffer".to_string());
    let args = [make_emacs_string(env, buffer)].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, get_buffer, 1, args) }
}
