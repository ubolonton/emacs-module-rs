use std::fmt::{self, Display};
use std::result;
use failure::{Context, Backtrace, Fail};
pub use failure::ResultExt;

use emacs_module::*;
use super::{Env, Value};
use super::IntoLisp;

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load. Possible or not?
pub type FuncallExit = emacs_funcall_exit;

const RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
const SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
const THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TempValue {
    raw: emacs_value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Fail)]
pub enum ErrorKind {
    #[fail(display = "Non-local signal: symbol={:?} data={:?}", symbol, data)]
    Signal { symbol: TempValue, data: TempValue },

    #[fail(display = "Non-local throw: tag={:?} value={:?}", tag, value)]
    Throw { tag: TempValue, value: TempValue },

    #[fail(display = "Wrong user-pointer type, expected: {}", expected)]
    UserPtrHasWrongType { expected: &'static str },

    #[fail(display = "Invalid user-pointer, expected: {}", expected)]
    UnknownUserPtr { expected: &'static str },

    #[fail(display = "Invalid symbol name")]
    InvalidSymbol,

    #[fail(display = "Invalid string")]
    InvalidString,

    #[fail(display = "Invalid function name")]
    InvalidFunction,

    #[fail(display = "Error from module {}", name)]
    Module { name: &'static str },

    #[fail(display = "Generic module error")]
    Generic,
}

#[derive(Debug)]
pub struct Error {
    inner: Context<ErrorKind>,
}

pub type Result<T> = result::Result<T, Error>;

impl TempValue {
    unsafe fn new(raw: emacs_value) -> Self {
        Self { raw }
    }

    /// # Safety
    ///
    /// This must only be temporarily used to inspect a non-local signal/throw from Lisp.
    pub unsafe fn value<'e>(&self, env: &'e Env) -> Value<'e> {
        Value::new(self.raw, env)
    }
}

/// Technically these are unsound, but they are necessary to use the `Fail` trait. We ensure safety
/// by marking TempValue methods as unsafe.
unsafe impl Send for TempValue {}
unsafe impl Sync for TempValue {}

impl Env {
    /// Handles possible non-local exit after calling Lisp code.
    pub(crate) fn handle_exit<T>(&self, result: T) -> Result<T> {
        // FIX: Can we not use .unwrap()?
        match self.non_local_exit_get() {
            (RETURN, ..) => Ok(result),
            (SIGNAL, symbol, data) => {
                self.non_local_exit_clear();
                Err(ErrorKind::Signal {
                    symbol: unsafe { TempValue::new(symbol) },
                    data: unsafe { TempValue::new(data) },
                }.into())
            },
            (THROW, tag, value) => {
                self.non_local_exit_clear();
                Err(ErrorKind::Throw {
                    tag: unsafe { TempValue::new(tag) },
                    value: unsafe { TempValue::new(value) },
                }.into())
            },
            (status, ..) => panic!("Unexpected non local exit status {}", status),
        }
    }

    /// Converts a Rust's `Result` to either a normal value, or a non-local exit in Lisp.
    pub(crate) unsafe fn maybe_exit(&self, result: Result<Value>) -> emacs_value {
        // FIX: Can we not use .unwrap()?
        match result {
            Ok(v) => v.raw,
            Err(error) => {
                match error.kind() {
                    ErrorKind::Signal { symbol, data } => return self.signal(
                        symbol.raw,
                        data.raw,
                    ),
                    ErrorKind::Throw { tag, value } => return self.throw(
                        tag.raw,
                        value.raw,
                    ),
                    // TODO: Internal
                    error => self.signal_str("error", &format!("Error: {}", error))
                        .expect("Fail to signal error to Emacs")
                }
            }
        }
    }

    // TODO: Prepare static values for the symbols.
    pub(crate) fn signal_str(&self, symbol: &str, message: &str) -> Result<emacs_value> {
        let message = message.into_lisp(&self)?;
        let data = self.list(&[message])?;
        let symbol = self.intern(symbol)?;
        unsafe {
            Ok(self.signal(symbol.raw, data.raw))
        }
    }

    fn non_local_exit_get(&self) -> (FuncallExit, emacs_value, emacs_value) {
        let mut buffer = Vec::<emacs_value>::with_capacity(2);
        let symbol = buffer.as_mut_ptr();
        let data = unsafe { symbol.offset(1) };
        let result = raw_call_no_exit!(self, non_local_exit_get, symbol, data);
        unsafe {
            (result, *symbol, *data)
        }
    }

    fn non_local_exit_clear(&self) {
        raw_call_no_exit!(self, non_local_exit_clear)
    }

    /// # Safety
    ///
    /// The given raw values must still live.
    #[allow(unused_unsafe)]
    unsafe fn throw(&self, tag: emacs_value, value: emacs_value) -> emacs_value {
        raw_call_no_exit!(self, non_local_exit_throw, tag, value);
        tag
    }

    /// # Safety
    ///
    /// The given raw values must still live.
    #[allow(unused_unsafe)]
    unsafe fn signal(&self, symbol: emacs_value, data: emacs_value) -> emacs_value {
        raw_call_no_exit!(self, non_local_exit_signal, symbol, data);
        symbol
    }
}

impl Error {
    pub fn kind(&self) -> ErrorKind {
        *self.inner.get_context()
    }
}

impl Fail for Error {
    fn cause(&self) -> Option<&Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error { inner: Context::new(kind) }
    }
}

impl From<Context<ErrorKind>> for Error {
    fn from(inner: Context<ErrorKind>) -> Error {
        Error { inner }
    }
}

impl From<Context<&'static str>> for Error {
    fn from(inner: Context<&'static str>) -> Error {
        ErrorKind::Module { name: inner.get_context() }.into()
    }
}
