use std::cell::RefCell;
use std::sync::{Mutex, RwLock};
use std::ffi::CString;
use libc;
use libc::ptrdiff_t;

use emacs_module::{emacs_runtime, emacs_env};
use super::{Env, Value};
use super::{Result};
use super::{ToLisp, FromLisp, IntoLisp, Transfer};

impl From<*mut emacs_env> for Env {
    fn from(raw: *mut emacs_env) -> Env {
        Env { raw }
    }
}

impl From<*mut emacs_runtime> for Env {
    fn from(runtime: *mut emacs_runtime) -> Env {
        let raw = unsafe {
            let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
            get_env(runtime)
        };
        Env { raw }
    }
}

impl ToLisp for i64 {
    fn to_lisp<'e>(&self, env: &'e Env) -> Result<Value<'e>> {
        raw_call_value!(env, make_integer, *self)
    }
}

// TODO: Make this more elegant. Can't implement it for trait bound Into<Vec<u8>>, since that would
// complain about conflicting implementations for i64.
impl ToLisp for str {
    fn to_lisp<'e>(&self, env: &'e Env) -> Result<Value<'e>> {
        let cstring = CString::new(self)?;
        let ptr = cstring.as_ptr();
        raw_call_value!(env, make_string, ptr, libc::strlen(ptr) as ptrdiff_t)
    }
}

impl FromLisp for i64 {
    fn from_lisp(value: &Value) -> Result<Self> {
        raw_call!(value.env, extract_integer, value.raw)
    }
}

impl FromLisp for String {
    // TODO: Optimize this.
    fn from_lisp(value: &Value) -> Result<Self> {
        let bytes = value.env.string_bytes(value)?;
        // FIX
        Ok(String::from_utf8(bytes).unwrap())
    }
}

impl<T: Transfer> IntoLisp for Box<T> {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let raw = Box::into_raw(self);
        let ptr = raw as *mut libc::c_void;
        raw_call_value!(env, make_user_ptr, Some(T::finalizer), ptr)
    }
}

enable_transfers! {
    RefCell;
    Mutex;
    RwLock;
}
