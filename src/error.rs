use std::result;
use std::error;
use std::io;
use std::ffi::NulError;

use emacs_module::*;
use super::{Env, Value};
use super::ToLisp;

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

impl Env {
    /// Handles possible non-local exit after calling Lisp code.
    pub(crate) fn handle_exit<T, U: Into<T>>(&self, result: U) -> Result<T> {
        match self.non_local_exit_get() {
            (RETURN, ..) => Ok(result.into()),
            (SIGNAL, symbol, data) => {
                self.non_local_exit_clear();
                Err(Error::signal(symbol, data))
            },
            (THROW, tag, value) => {
                self.non_local_exit_clear();
                Err(Error::throw(tag, value))
            },
            // TODO: Don't panic here, use a custom error.
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }

    /// Converts a Rust's `Result` to either a normal value, or a non-local exit in Lisp.
    pub(crate) unsafe fn maybe_exit(&self, result: Result<Value>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(normal_error) => {
                match normal_error.kind {
                    ErrorKind::Signal { symbol, data } => self.signal(symbol, data),
                    ErrorKind::Throw { tag, value } => self.throw(tag, value),
                    // TODO: Better formatting.
                    other_error => self.signal_str("error", &format!("Error: {:#?}", other_error))
                        .expect("Fail to signal error to Emacs"),
                }
            }
        }
    }

    // TODO: Prepare static values for the symbols.
    pub(crate) fn signal_str(&self, symbol: &str, message: &str) -> Result<emacs_value> {
        let message = message.to_lisp(&self)?;
        let data = self.list(&[message])?;
        let symbol = self.intern(symbol)?;
        Ok(self.signal(symbol.raw, data.raw))
    }

    fn non_local_exit_get(&self) -> (FuncallExit, emacs_value, emacs_value) {
        let mut buffer = Vec::<emacs_value>::with_capacity(2);
        let symbol = buffer.as_mut_ptr();
        let data = unsafe { symbol.offset(1) };
        let result = critical!(self, non_local_exit_get, symbol, data);
        unsafe {
            (result, *symbol, *data)
        }
    }

    fn non_local_exit_clear(&self) {
        critical!(self, non_local_exit_clear)
    }

    fn throw(&self, tag: emacs_value, value: emacs_value) -> emacs_value {
        critical!(self, non_local_exit_throw, tag, value);
        tag
    }

    fn signal(&self, symbol: emacs_value, data: emacs_value) -> emacs_value {
        critical!(self, non_local_exit_signal, symbol, data);
        symbol
    }
}
