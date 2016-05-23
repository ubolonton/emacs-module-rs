pub mod emacs_module;

extern crate libc;
use emacs_module::{emacs_env, emacs_value, Struct_emacs_value_tag, Struct_emacs_runtime, Struct_emacs_env_25};
use std::ffi::CString;
use std::os::raw;

pub type EmacsFunction = Option<unsafe extern "C" fn(*mut Struct_emacs_env_25, i64, *mut *mut Struct_emacs_value_tag, *mut raw::c_void) -> *mut Struct_emacs_value_tag>;

#[no_mangle]
pub extern fn find_function(env: * mut emacs_env, name: String) -> * mut Struct_emacs_value_tag {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern fn make_function(env: * mut emacs_env, min_args: i64, max_args: i64, f: EmacsFunction, doc: String, user_ptr: *mut raw::c_void) -> *mut Struct_emacs_value_tag {
    let doc = CString::new(doc).unwrap().as_ptr();
    unsafe { (*env).make_function.unwrap()(env, min_args, max_args, f, doc, user_ptr) }
}

#[no_mangle]
pub extern fn get_environment(ert: * mut Struct_emacs_runtime) -> * mut Struct_emacs_env_25 {
    unsafe { (*ert).get_environment.unwrap()(ert) }
}

#[no_mangle]
pub extern fn intern_symbol(env: * mut emacs_env, name: String) -> * mut Struct_emacs_value_tag {
    unsafe { (*env).intern.unwrap()(env, CString::new(name).unwrap().as_ptr()) }
}

#[no_mangle]
pub extern fn bind_function(env: * mut emacs_env, name: String, sfun: emacs_value) {
    let qfset = find_function(env, "fset".to_string());
    let qsym = intern_symbol(env, name);
    let args = [qsym, sfun].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, qfset, 2, args) };
}

#[no_mangle]
pub extern fn provide(env: * mut emacs_env, feature: * const libc::c_char) {
    let feat = unsafe { (*env).intern.unwrap()(env, feature) };
    let provide = find_function(env, "provide".to_string());
    let args = [feat].as_mut_ptr();
    unsafe { (*env).funcall.unwrap()(env, provide, 1, args) };
}
