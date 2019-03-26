use std::mem;
use std::result;
use std::thread;
use failure_derive::Fail;
pub use failure::{Error, ResultExt};

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

#[derive(Debug)]
pub struct TempValue {
    raw: emacs_value,
}

const WRONG_TYPE_USER_PTR: &str = "rust-wrong-type-user-ptr";
const ERROR: &str = "rust-error";
const PANIC: &str = "rust-panic";

#[derive(Debug, Fail)]
pub enum ErrorKind {
    #[fail(display = "Non-local signal: symbol={:?} data={:?}", symbol, data)]
    Signal { symbol: TempValue, data: TempValue },

    #[fail(display = "Non-local throw: tag={:?} value={:?}", tag, value)]
    Throw { tag: TempValue, value: TempValue },

    #[fail(display = "Wrong type user-ptr, expected: {}", expected)]
    WrongTypeUserPtr { expected: &'static str },
}

pub type Result<T> = result::Result<T, Error>;

// FIX: Make this into RootedValue (or ProtectedValue), and make it safe. XXX: The problem is that
// the raw value will be leaked when RootedValue is dropped, since `free_global_ref` requires an env
// (thus cannot be called there). This is likely a mis-design in Emacs (In Erlang,
// `enif_keep_resource` and `enif_release_resource` don't require an env).
impl TempValue {
    unsafe fn new(raw: emacs_value) -> Self {
        Self { raw }
    }

    /// # Safety
    ///
    /// This must only be used with the [`Env`] from which the error originated.
    ///
    /// [`Env`]: struct.Env.html
    pub unsafe fn value<'e>(&self, env: &'e Env) -> Value<'e> {
        Value::new_protected(self.raw, env)
    }
}

/// Technically these are unsound, but they are necessary to use the `Fail` trait. We ensure safety
/// by marking TempValue methods as unsafe.
unsafe impl Send for TempValue {}
unsafe impl Sync for TempValue {}

impl Env {
    /// Handles possible non-local exit after calling Lisp code.
    pub(crate) fn handle_exit<T>(&self, result: T) -> Result<T> {
        let mut symbol = unsafe { mem::uninitialized() };
        let mut data = unsafe { mem::uninitialized() };
        let status = self.non_local_exit_get(&mut symbol, &mut data);
        match (status, symbol, data) {
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
            _ => panic!("Unexpected non local exit status {}", status),
        }
    }

    /// Converts a Rust's `Result` to either a normal value, or a non-local exit in Lisp.
    pub(crate) unsafe fn maybe_exit(&self, result: Result<Value<'_>>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(error) => {
                match error.downcast_ref::<ErrorKind>() {
                    Some(&ErrorKind::Signal { ref symbol, ref data }) =>
                        self.signal(symbol.raw, data.raw),
                    Some(&ErrorKind::Throw { ref tag, ref value }) =>
                        self.throw(tag.raw, value.raw),
                    Some(&ErrorKind::WrongTypeUserPtr { .. }) =>
                        self.signal_str(WRONG_TYPE_USER_PTR, &format!("{}", error))
                        .expect(&format!("Failed to signal {}", error)),
                    _ => self.signal_str(ERROR, &format!("{}", error))
                        .expect(&format!("Failed to signal {}", error)),
                }
            }
        }
    }

    pub(crate) fn handle_panic(&self, result: thread::Result<emacs_value>) -> emacs_value {
        match result {
            Ok(v) => v,
            Err(error) => {
                // TODO: Try to check for some common types to display?
                self.signal_str(PANIC, &format!("{:#?}", error))
                    .expect(&format!("Fail to signal panic {:#?}", error))
            },
        }
    }

    pub(crate) fn define_errors(&self) -> Result<()> {
        // FIX: Make panics louder than errors, by somehow make sure that 'rust-panic is
        // not a sub-type of 'error.
        self.define_error(PANIC, "Rust panic", "error")?;
        self.define_error(ERROR, "Rust error", "error")?;
        // TODO: This should also be a sub-types of 'wrong-type-argument?
        self.define_error(WRONG_TYPE_USER_PTR, "Wrong type user-ptr", ERROR)?;
        Ok(())
    }

    // TODO: Prepare static values for the symbols.
    fn signal_str(&self, symbol: &str, message: &str) -> Result<emacs_value> {
        let message = message.into_lisp(&self)?;
        let data = self.list(&[message])?;
        let symbol = self.intern(symbol)?;
        unsafe {
            Ok(self.signal(symbol.raw, data.raw))
        }
    }

    fn define_error(&self, name: &str, message: &str, parent: &str) -> Result<Value<'_>> {
        self.call("define-error", &[
            self.intern(name)?,
            message.into_lisp(self)?,
            self.intern(parent)?
        ])
    }

    fn non_local_exit_get(&self, symbol: &mut emacs_value, data: &mut emacs_value) -> FuncallExit {
        raw_call_no_exit!(self, non_local_exit_get, symbol as *mut emacs_value, data as *mut emacs_value)
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
