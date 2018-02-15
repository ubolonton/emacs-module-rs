use std::ops::Range;
use std::ffi::CString;
use libc;

use emacs_module::{EmacsSubr};
use super::{Env, CallEnv, Value};
use super::error::Result;

pub type Func = fn(env: &CallEnv) -> Result<Value>;

pub type InitFunc = fn(env: &Env) -> Result<Value>;

// TODO: This should be named sth like HandleSubr, HandleRawFn
// TODO: Enable creating a Lisp function from a Rust fn. That probably requires procedural macros,
// macro_rules! is inadequate.
pub trait HandleFunc {
    fn make_function<T>(&self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void) -> Result<Value> where T: Into<Vec<u8>>;
    fn fset(&self, name: &str, func: Value) -> Result<Value>;
}

impl HandleFunc for Env {
    fn make_function<T>(&self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void) -> Result<Value> where T: Into<Vec<u8>> {
        raw_call_value!(
            self, make_function,
            arities.start as isize, arities.end as isize,
            Some(function), CString::new(doc)?.as_ptr(), data
        )
    }

    fn fset(&self, name: &str, func: Value) -> Result<Value> {
        let symbol = self.intern(name)?;
        call_lisp!(self, "fset", symbol, func)
    }
}
