use std::ops::{Range, Deref};
use std::panic;
use std::slice;
use std::ffi::CString;
use libc;

use emacs_module::{EmacsSubr, emacs_value};
use super::{Env, CallEnv, Value, Result};
use super::FromLisp;

pub type Func = fn(env: &CallEnv) -> Result<Value>;

pub type InitFunc = fn(env: &Env) -> Result<Value>;

pub trait Manage {
    fn make_function<T>(&self, function: EmacsSubr, arities: Range<usize>, doc: T, data: *mut libc::c_void) -> Result<Value> where T: Into<Vec<u8>>;

    fn fset(&self, name: &str, func: Value) -> Result<Value>;
}

pub trait HandleInit {
    fn handle_init(self, f: InitFunc) -> libc::c_int;
}

pub trait HandleCall {
    fn handle_call(self, f: Func) -> emacs_value;
}

impl Manage for Env {
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

impl HandleInit for Env {
    fn handle_init(self, f: InitFunc) -> libc::c_int {
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
        self.get_arg(i).to_rust()
    }
}

impl HandleCall for CallEnv {
    fn handle_call(self, f: Func) -> emacs_value {
        let result = panic::catch_unwind(|| {
            unsafe {
                self.maybe_exit(f(&self))
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
