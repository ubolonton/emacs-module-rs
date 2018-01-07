use std::result;
use std::error;
use std::io;
use std::ffi::NulError;
use emacs_module::*;
use super::{Env, Value, ToEmacs};

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
    Signal { symbol: Value, data: Value },
    Throw { tag: Value, value: Value },
    UserPtrHasWrongType { expected: &'static str },
    UnknownUserPtr { expected: &'static str },
    IO { error: io::Error },
    Other { error: Box<error::Error> },
    CoreFnMissing(String),
}

pub type Result<T> = result::Result<T, Error>;

impl Error {
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn new<E: Into<Box<error::Error>>>(error: E) -> Self {
        Self { kind: ErrorKind::Other { error: error.into() } }
    }

    // TODO: Public version of signal/throw that take ToEmacs values.
    fn signal(symbol: Value, data: Value) -> Self {
        Self { kind: ErrorKind::Signal { symbol, data } }
    }

    fn throw(tag: Value, value: Value) -> Self {
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

pub(crate) trait HandleExit {
    fn handle_exit<T, U: Into<T>>(&self, result: U) -> Result<T>;
}

fn non_local_exit_get(env: &Env) -> (FuncallExit, Value, Value) {
    let mut buffer = Vec::<emacs_value>::with_capacity(2);
    let symbol = buffer.as_mut_ptr();
    let data = unsafe { symbol.offset(1) };
    let result = critical!(env, non_local_exit_get, symbol, data);
    unsafe {
        (result, (*symbol).into(), (*data).into())
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
                // TODO: Shouldn't we call make_global_ref here to make sure symbol and data are
                // not GC'ed? Maybe in a wrapper type that calls free_global_ref when dropped.
                // The only issue is that free_global_ref requires emacs_env (even though it
                // doesn't currently use that).
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

// TODO: Use these only in the wrapper funcs that give the error back to Emacs. One problem is,
// wrappers are written (by macros) by user code, which shouldn't have access to these.
pub trait TriggerExit {
    fn maybe_exit(&mut self, result: Result<Value>) -> emacs_value;
}

fn throw(env: &mut Env, tag: Value, value: Value) -> emacs_value {
    let (tag, value) = (tag.raw, value.raw);
    critical!(env, non_local_exit_throw, tag, value);
    tag
}

fn signal(env: &mut Env, symbol: Value, data: Value) -> emacs_value {
    let (symbol, data) = (symbol.raw, data.raw);
    critical!(env, non_local_exit_signal, symbol, data);
    symbol
}

// XXX
fn error(env: &mut Env, message: &str) -> Result<emacs_value> {
    let message = message.to_emacs(env)?;
    let data = env.list(&mut [message])?.into();
    let symbol = env.intern("error")?.into();
    Ok(signal(env, symbol, data))
}

impl TriggerExit for Env {
    /// This is intended to be used at the Rust->Emacs boundary, by the internal macros/functions.
    /// Module code should use [`Error::throw`] and [`Error::signal`] instead.
    fn maybe_exit(&mut self, result: Result<Value>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(normal_error) => {
                match normal_error.kind {
                    ErrorKind::Signal { symbol, data } => signal(self, symbol, data),
                    ErrorKind::Throw { tag, value } => throw(self, tag, value),
                    // TODO: Better formatting.
                    other_error => match error(self, &format!("Error: {:#?}", other_error)) {
                        Ok(v) => v,
                        // XXX: Custom error instead of panicking.
                        Err(_fail_to_error) => {
                            error(self, "Undisplayable error")
                                .expect("Fail to signal error to Emacs")
                        }
                    },
                }
            }
        }
    }
}
