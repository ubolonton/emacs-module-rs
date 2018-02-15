use std::panic;
use std::any::Any;
use std::result;
use std::error;
use std::io;
use std::ffi::NulError;
use libc;

use emacs_module::*;
use super::{Env, CallEnv, Value, ToLisp};
use super::func::{Func, InitFunc};

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load. Possible or not?
pub type FuncallExit = emacs_funcall_exit;

const RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
const SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
const THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

#[derive(Debug)]
pub struct Error {
    pub(crate) kind: ErrorKind,
}

// TODO: Use error-chain? (need to solve the issue that emacs_value does not satisfy Send).
#[derive(Debug)]
pub enum ErrorKind {
    // TODO: Define RootedValue or OwnedValue or something.
    Signal { symbol: emacs_value, data: emacs_value },
    Throw { tag: emacs_value, value: emacs_value },
    UserPtrHasWrongType { expected: &'static str },
    UnknownUserPtr { expected: &'static str },
    IO { error: io::Error },
    Other { error: Box<error::Error> },
    Panic { error: Box<Any + Send + 'static> },
    CoreFnMissing(String),
}

pub type Result<T> = result::Result<T, Error>;

pub(crate) trait HandleExit {
    fn handle_exit<T, U: Into<T>>(&self, result: U) -> Result<T>;
}

trait TriggerExit {
    unsafe fn maybe_exit(&self, result: Result<Value>) -> emacs_value;
}

pub trait HandleInit {
    unsafe fn handle_init(&self, f: InitFunc) -> libc::c_int;
}

pub trait HandleCall {
    unsafe fn handle_call(&self, f: Func) -> emacs_value;
}

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn new<E: Into<Box<error::Error>>>(error: E) -> Self {
        Self { kind: ErrorKind::Other { error: error.into() } }
    }

    // TODO: Public version of signal/throw that take ToEmacs values.
    fn signal(symbol: emacs_value, data: emacs_value) -> Self {
        Self { kind: ErrorKind::Signal { symbol, data } }
    }

    fn throw(tag: emacs_value, value: emacs_value) -> Self {
        Self { kind: ErrorKind::Throw { tag, value } }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self { kind }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        ErrorKind::IO { error }.into()
    }
}

// TODO: Better reporting.
impl From<NulError> for Error {
    fn from(error: NulError) -> Self {
        ErrorKind::Other { error: Box::new(error) }.into()
    }
}

fn non_local_exit_get(env: &Env) -> (FuncallExit, emacs_value, emacs_value) {
    let mut buffer = Vec::<emacs_value>::with_capacity(2);
    let symbol = buffer.as_mut_ptr();
    let data = unsafe { symbol.offset(1) };
    let result = critical!(env, non_local_exit_get, symbol, data);
    unsafe {
        (result, *symbol, *data)
    }
}

fn non_local_exit_clear(env: &Env) {
    critical!(env, non_local_exit_clear)
}

impl HandleExit for Env {
    fn handle_exit<T, U: Into<T>>(&self, result: U) -> Result<T> {
        match non_local_exit_get(self) {
            (RETURN, ..) => Ok(result.into()),
            (SIGNAL, symbol, data) => {
                non_local_exit_clear(self);
                Err(Error::signal(symbol, data))
            },
            (THROW, tag, value) => {
                non_local_exit_clear(self);
                Err(Error::throw(tag, value))
            },
            // TODO: Don't panic here, use a custom error.
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }
}

fn throw(env: &Env, tag: emacs_value, value: emacs_value) -> emacs_value {
    critical!(env, non_local_exit_throw, tag, value);
    tag
}

fn signal(env: &Env, symbol: emacs_value, data: emacs_value) -> emacs_value {
    critical!(env, non_local_exit_signal, symbol, data);
    symbol
}

// TODO: Prepare static values for the symbols.
fn _signal(env: &Env, symbol: &str, message: &str) -> Result<emacs_value> {
    let message = message.to_lisp(env)?;
    let data = env.list(&[message])?;
    let symbol = env.intern(symbol)?;
    Ok(signal(env, symbol.raw, data.raw))
}

impl TriggerExit for Env {
    /// This is intended to be used at the Rust->Emacs boundary, by the internal macros/functions.
    /// Module code should use [`Error::throw`] and [`Error::signal`] instead.
    unsafe fn maybe_exit(&self, result: Result<Value>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(normal_error) => {
                match normal_error.kind {
                    ErrorKind::Signal { symbol, data } => signal(self, symbol, data),
                    ErrorKind::Throw { tag, value } => throw(self, tag, value),
                    // TODO: Better formatting.
                    other_error => _signal(self, "error", &format!("Error: {:#?}", other_error))
                        .expect("Fail to signal error to Emacs"),
                }
            }
        }
    }
}

impl HandleInit for Env {
    unsafe fn handle_init(&self, f: InitFunc) -> libc::c_int {
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

impl HandleCall for CallEnv {
    unsafe fn handle_call(&self, f: Func) -> emacs_value {
        let result = panic::catch_unwind(|| {
            self.maybe_exit(f(&self))
        });
        match result {
            Ok(v) => v,
            Err(e) => {
                // TODO: Try some common types
                // TODO: Get stack trace?
                // TODO: Just exit?
                _signal(self, "panic", &format!("{:#?}", e))
                    .expect("Fail to signal panic to Emacs")
            },
        }
    }
}
