//! This module contains additional methods on [`Env`] that are mainly used by macros. User code
//! should use these macros instead of this module.
//!
//! [`Env`]: struct.Env.html

use std::ops::{Range, Deref};
use std::panic;
use std::slice;
use std::ffi::CString;
use libc;

use emacs_module::{EmacsSubr, emacs_value};
use super::{Env, CallEnv, Value};
use super::{FromLisp, IntoLisp};
use super::error::Result;

pub trait Manage {
    fn make_function<T: Into<Vec<u8>>>(
        &self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void
    ) -> Result<Value<'_>>;

    fn fset(&self, name: &str, func: Value<'_>) -> Result<Value<'_>>;
}

pub trait HandleInit {
    fn handle_init<F>(self, f: F) -> libc::c_int
    where F: Fn(&Env) -> Result<Value<'_>> + panic::RefUnwindSafe;
}

pub trait HandleCall {
    fn handle_call<'e, T, F>(&'e self, f: F) -> emacs_value
    where
        F: Fn(&'e CallEnv) -> Result<T> + panic::RefUnwindSafe,
        T: IntoLisp<'e>;
}

impl Manage for Env {
    fn make_function<T: Into<Vec<u8>>>(
        &self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void
    ) -> Result<Value<'_>> {
        raw_call_value!(
            self, make_function,
            arities.start as isize, arities.end as isize,
            Some(function), CString::new(doc)?.as_ptr(), data
        )
    }

    fn fset(&self, name: &str, func: Value<'_>) -> Result<Value<'_>> {
        let symbol = self.intern(name)?;
        call_lisp!(self, "fset", symbol, func)
    }
}

impl HandleInit for Env {
    fn handle_init<F>(self, f: F) -> libc::c_int
    where F: Fn(&Env) -> Result<Value<'_>> + panic::RefUnwindSafe
    {
        let env = panic::AssertUnwindSafe(self);
        let result = panic::catch_unwind(|| {
            match env.define_errors().and_then(|_| f(&env)) {
                Ok(_) => 0,
                Err(e) => {
                    env.message(&format!("Error during initialization: {:#?}", e))
                        .expect("Fail to message Emacs about error");
                    1
                },
            }
        });
        match result {
            Ok(v) => v,
            Err(e) => {
                env.message(&format!("Panic during initialization: {:#?}", e))
                    .expect("Fail to message Emacs about panic");
                2
            },
        }
    }
}

// TODO: Iterator and Index
impl CallEnv {
    pub unsafe fn new(env: Env,
                      nargs: libc::ptrdiff_t,
                      args: *mut emacs_value,
                      data: *mut libc::c_void) -> Self {
        let nargs = nargs as usize;
        Self { env, nargs, args, data }
    }

    pub fn raw_args(&self) -> &[emacs_value] {
        unsafe {
            slice::from_raw_parts(self.args, self.nargs)
        }
    }

    pub fn args(&self) -> Vec<Value<'_>> {
        self.raw_args().iter().map(|v| unsafe {
            Value::new(*v, &self.env)
        }).collect()
    }

    pub fn get_arg(&self, i: usize) -> Value<'_> {
        let args: &[emacs_value] = self.raw_args();
        unsafe {
            Value::new(args[i], &self)
        }
    }

    pub fn parse_arg<'e, T: FromLisp<'e>>(&'e self, i: usize) -> Result<T> {
        self.get_arg(i).into_rust()
    }
}

impl HandleCall for CallEnv {
    fn handle_call<'e, T, F>(&'e self, f: F) -> emacs_value
    where
        F: Fn(&'e CallEnv) -> Result<T> + panic::RefUnwindSafe,
        T: IntoLisp<'e>,
    {
        let env = panic::AssertUnwindSafe(self);
        let result = panic::catch_unwind(|| {
            unsafe {
                let rust_result = f(&env);
                let lisp_result = rust_result.and_then(|t| t.into_lisp(&env));
                env.maybe_exit(lisp_result)
            }
        });
        env.handle_panic(result)
    }
}

/// This allows `Env`'s methods to be called on a `CallEnv`.
impl Deref for CallEnv {
    type Target = Env;

    fn deref(&self) -> &Env {
        &self.env
    }
}
