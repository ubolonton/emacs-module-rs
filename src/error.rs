use std::fmt::{self, Display};
use std::result;

use emacs_module::*;
use super::{Env, Value};
use super::IntoLisp;
use super::RootedValue;

pub use failure::Error;

/// We assume that the C code in Emacs really treats it as an enum and doesn't return an undeclared
/// value, but we still need to safeguard against possible compatibility issue (Emacs may add more
/// statuses in the future). FIX: Use an enum, and check for compatibility on load. Possible or not?
pub type FuncallExit = emacs_funcall_exit;

const RETURN: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_return;
const SIGNAL: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_signal;
const THROW: FuncallExit = emacs_funcall_exit_emacs_funcall_exit_throw;

#[derive(Debug, Fail)]
pub enum NonLocal {
    Signal { symbol: RootedValue, data: RootedValue },
    Throw { tag: RootedValue, value: RootedValue },
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Fail)]
pub enum Internal {
    UserPtrHasWrongType { expected: &'static str },
    UnknownUserPtr { expected: &'static str },
    // TODO: Just panic? We have `catch_unwind`.
    CoreFnMissing(&'static str),
}

pub type Result<T> = result::Result<T, Error>;

// TODO
impl Display for Internal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", &self)
    }
}

// TODO
impl Display for NonLocal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", &self)
    }
}

impl Env {
    /// Handles possible non-local exit after calling Lisp code.
    pub(crate) fn handle_exit<T>(&self, result: T) -> Result<T> {
        // FIX: Can we not use .unwrap()?
        match self.non_local_exit_get() {
            (RETURN, ..) => Ok(result),
            (SIGNAL, symbol, data) => {
                self.non_local_exit_clear();
                Err(NonLocal::Signal {
                    symbol: RootedValue::new(symbol, self).unwrap(),
                    data: RootedValue::new(data, self).unwrap(),
                }.into())
            },
            (THROW, tag, value) => {
                self.non_local_exit_clear();
                Err(NonLocal::Throw {
                    tag: RootedValue::new(tag, self).unwrap(),
                    value: RootedValue::new(value, self).unwrap(),
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
                let error = match error.downcast::<NonLocal>() {
                    Ok(NonLocal::Signal { symbol, data }) => return self.signal(
                        symbol.uproot(self).unwrap(),
                        data.uproot(self).unwrap(),
                    ),
                    Ok(NonLocal::Throw { tag, value }) => return self.throw(
                        tag.uproot(self).unwrap(),
                        value.uproot(self).unwrap(),
                    ),
                    Err(error) => error,
                };
                // TODO: Internal
                self.signal_str("error", &format!("Error: {}", error))
                    .expect("Fail to signal error to Emacs")
            }
        }
    }

    // TODO: Prepare static values for the symbols.
    pub(crate) fn signal_str(&self, symbol: &str, message: &str) -> Result<emacs_value> {
        let message = message.into_lisp(&self)?;
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
