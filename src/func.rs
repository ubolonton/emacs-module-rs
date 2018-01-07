use std::ffi::CString;
use std::ops::Range;
use libc;
use emacs_module::{EmacsSubr};
use super::{Env, Value};
use super::error::Result;

// TODO: This should be named sth like HandleSubr, HandleRawFn
// TODO: Enable creating a Lisp function from a Rust fn. That probably requires procedural macros,
// macro_rules! is inadequate.
pub trait HandleFunc {
    fn make_function(&mut self, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<Value>;
    fn fset(&mut self, name: &str, func: Value) -> Result<Value>;
    fn register(&mut self, name: &str, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<Value>;
}

impl HandleFunc for Env {
    fn make_function(&mut self, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<Value> {
        raw_call!(
            self, make_function,
            arities.start as isize, arities.end as isize,
            Some(function), CString::new(doc)?.as_ptr(), data
        )
    }

    fn fset(&mut self, name: &str, func: Value) -> Result<Value> {
        let symbol = self.intern(name)?;
        self.call("fset", &[symbol, func])
    }

    fn register(&mut self, name: &str, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<Value> {
        let function = self.make_function(function, arities, doc, data)?;
        self.fset(name, function)
    }
}
