use std::result;
use std::error;
use std::io;
use std::ffi::NulError;
use emacs_module::*;
use super::{Env, ToEmacs};

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load. Possible or not?
pub type FuncallExit = emacs_funcall_exit;

const RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
const SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
const THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

#[derive(Debug)]
pub struct Error {
    pub(crate) kind: ErrorKind
}

// TODO: Use error-chain? (need to solve the issue that EmacsVal does not satisfy Send).
#[derive(Debug)]
pub enum ErrorKind {
    Signal { symbol: EmacsVal, data: EmacsVal },
    Throw { tag: EmacsVal, value: EmacsVal },
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
    fn signal(symbol: EmacsVal, data: EmacsVal) -> Self {
        Self { kind: ErrorKind::Signal { symbol, data } }
    }

    fn throw(tag: EmacsVal, value: EmacsVal) -> Self {
        Self { kind: ErrorKind::Throw { tag, value } }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self { kind: ErrorKind::IO { error } }
    }
}

// TODO: Better reporting.
impl From<NulError> for Error {
    fn from(error: NulError) -> Self {
        Self { kind: ErrorKind::Other { error: Box::new(error) } }
    }
}

pub(crate) trait HandleExit {
    fn handle_exit<T>(&self, result: T) -> Result<T>;
}

/// Note: Some functions in emacs-module.h are critically important, like those that support error
/// reporting to Emacs. If they are missing, the only sensible thing to do is crashing. Use this
/// macro to call them instead of [`raw_call!`].
macro_rules! critical {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        unsafe {
            let $name = raw_fn!($env, $name)
                .expect(&format!("Required function {} cannot be found", stringify!($name)));
            $name($env.raw $(, $args)*)
        }
    };
}

fn non_local_exit_get(env: &Env) -> (FuncallExit, EmacsVal, EmacsVal) {
    let mut buffer = Vec::<EmacsVal>::with_capacity(2);
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
    fn handle_exit<T>(&self, result: T) -> Result<T> {
        match non_local_exit_get(self) {
            (RETURN, ..) => Ok(result),
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
    fn maybe_exit(&mut self, result: Result<EmacsVal>) -> EmacsVal;
}

fn throw(env: &mut Env, tag: EmacsVal, value: EmacsVal) -> EmacsVal {
    critical!(env, non_local_exit_throw, tag, value);
    tag
}

fn signal(env: &mut Env, symbol: EmacsVal, data: EmacsVal) -> EmacsVal {
    critical!(env, non_local_exit_signal, symbol, data);
    symbol
}

// XXX
fn error(env: &mut Env, message: &str) -> Result<EmacsVal> {
    let message = message.to_emacs(env)?;
    let data = env.list(&mut [message])?;
    let symbol = env.intern("error")?;
    Ok(signal(env, symbol, data))
}

impl TriggerExit for Env {
    /// This is intended to be used at the Rust->Emacs boundary, by the internal macros/functions.
    /// Module code should use [`Error::throw`] and [`Error::signal`] instead.
    fn maybe_exit(&mut self, result: Result<EmacsVal>) -> EmacsVal {
        match result {
            Ok(v) => v,
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
