use std::ops::{Range, Deref};
use std::panic;
use std::slice;
use std::ffi::CString;
use libc;

use emacs_module::{EmacsSubr, emacs_value};
use super::{Env, CallEnv, Value};
use super::{FromLisp, IntoLisp};
use super::error::{ErrorKind, Result, ResultExt};

pub trait Manage {
    fn make_function<T: Into<Vec<u8>>>(
        &self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void
    ) -> Result<Value>;

    fn fset(&self, name: &str, func: Value) -> Result<Value>;
}

pub trait HandleInit {
    fn handle_init<F>(self, f: F) -> libc::c_int
    where F: Fn(&Env) -> Result<Value> + panic::RefUnwindSafe;
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
    ) -> Result<Value> {
        raw_call_value!(
            self, make_function,
            arities.start as isize, arities.end as isize,
            Some(function), CString::new(doc).context(ErrorKind::InvalidFunction)?.as_ptr(), data
        )
    }

    fn fset(&self, name: &str, func: Value) -> Result<Value> {
        let symbol = self.intern(name)?;
        call_lisp!(self, "fset", symbol, func)
    }
}

impl HandleInit for Env {
    fn handle_init<F>(self, f: F) -> libc::c_int
    where F: Fn(&Env) -> Result<Value> + panic::RefUnwindSafe
    {
        let result = panic::catch_unwind(|| {
            match f(&self) {
                Ok(_) => 0,
                Err(e) => {
                    self.message(&format!("Error during initialization: {:#?}", e))
                        .expect("Fail to message Emacs about error");
                    1
                },
            }
        });
        match result {
            Ok(v) => v,
            Err(e) => {
                // TODO: Try some common types
                // TODO: Get stack trace?
                // TODO: Just exit?
                self.message(&format!("Panic during initialization: {:#?}", e))
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

    pub fn args(&self) -> Vec<Value> {
        self.raw_args().iter().map(|v| unsafe {
            Value::new(*v, &self.env)
        }).collect()
    }

    pub fn get_arg(&self, i: usize) -> Value {
        let args: &[emacs_value] = self.raw_args();
        unsafe {
            Value::new(args[i], &self)
        }
    }

    pub fn parse_arg<T: FromLisp>(&self, i: usize) -> Result<T> {
        self.get_arg(i).into_rust()
    }
}

impl HandleCall for CallEnv {
    fn handle_call<'e, T, F>(&'e self, f: F) -> emacs_value
    where
        F: Fn(&'e CallEnv) -> Result<T> + panic::RefUnwindSafe,
        T: IntoLisp<'e>,
    {
        let result = panic::catch_unwind(|| {
            unsafe {
                let x = f(&self);
                let y = x.and_then(|t| t.into_lisp(&self));
                self.maybe_exit(y)
            }
        });
        match result {
            Ok(v) => v,
            Err(e) => {
                // TODO: Try some common types
                // TODO: Get stack trace?
                // TODO: Just exit?
                self.signal_str("panic", &format!("{:#?}", e))
                    .expect("Fail to signal panic to Emacs")
            },
        }
    }
}

/// This allows `Env`'s methods to be called on a `CallEnv`.
impl Deref for CallEnv {
    type Target = Env;

    fn deref(&self) -> &Env {
        &self.env
    }
}
