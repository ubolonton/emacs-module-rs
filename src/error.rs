#[doc(no_inline)]
pub use anyhow::{self, Error};
use std::mem::MaybeUninit;
use std::result;
use std::thread;
use std::any::Any;
use thiserror::Error;

use super::IntoLisp;
use super::{Env, Value};
use emacs_module::*;

// We use const instead of enum, in case Emacs add more exit statuses in the future.
// See https://github.com/rust-lang/rust/issues/36927
pub(crate) const RETURN: emacs_funcall_exit = emacs_funcall_exit_return;
pub(crate) const SIGNAL: emacs_funcall_exit = emacs_funcall_exit_signal;
pub(crate) const THROW: emacs_funcall_exit = emacs_funcall_exit_throw;

#[derive(Debug)]
pub struct TempValue {
    raw: emacs_value,
}

const WRONG_TYPE_USER_PTR: &str = "rust-wrong-type-user-ptr";
const ERROR: &str = "rust-error";
const PANIC: &str = "rust-panic";

/// Error types generic to all Rust dynamic modules.
///
/// This list is intended to grow over time and it is not recommended to exhaustively match against
/// it.
#[derive(Debug, Error)]
pub enum ErrorKind {
    /// An [error] signaled by Lisp code.
    ///
    /// [error]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Signaling-Errors.html
    #[error("Non-local signal: symbol={symbol:?} data={data:?}")]
    Signal { symbol: TempValue, data: TempValue },

    /// A [non-local exit] thrown by Lisp code.
    ///
    /// [non-local exit]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Catch-and-Throw.html
    #[error("Non-local throw: tag={tag:?} value={value:?}")]
    Throw { tag: TempValue, value: TempValue },

    /// An error indicating that the given value is not a `user-ptr` of the expected type.
    ///
    /// # Examples:
    ///
    /// ```
    /// # use emacs::*;
    /// # use std::cell::RefCell;
    /// #[defun]
    /// fn wrap(x: i64) -> Result<RefCell<i64>> {
    ///     Ok(RefCell::new(x))
    /// }
    ///
    /// #[defun]
    /// fn wrap_f(x: f64) -> Result<RefCell<f64>> {
    ///     Ok(RefCell::new(x))
    /// }
    ///
    /// #[defun]
    /// fn unwrap(r: &RefCell<i64>) -> Result<i64> {
    ///     Ok(*r.try_borrow()?)
    /// }
    /// ```
    ///
    /// ```emacs-lisp
    /// (unwrap 7)          ; *** Eval error ***  Wrong type argument: user-ptrp, 7
    /// (unwrap (wrap 7))   ; 7
    /// (unwrap (wrap-f 7)) ; *** Eval error ***  Wrong type user-ptr: "expected: RefCell"
    /// ```
    #[error("expected: {expected}")]
    WrongTypeUserPtr { expected: &'static str },
}

/// A specialized [`Result`] type for Emacs's dynamic modules.
///
/// [`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
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
        Value::new(self.raw, env).protect()
    }
}

// XXX: Technically these are unsound, but they are necessary to use the `Fail` trait. We ensure
// safety by marking TempValue methods as unsafe.
unsafe impl Send for TempValue {}
unsafe impl Sync for TempValue {}

impl Env {
    /// Handles possible non-local exit after calling Lisp code.
    #[inline]
    pub(crate) fn handle_exit<T>(&self, result: T) -> Result<T> {
        let mut symbol = MaybeUninit::uninit();
        let mut data = MaybeUninit::uninit();
        // TODO: Check whether calling non_local_exit_check first makes a difference in performance.
        let status = self.non_local_exit_get(&mut symbol, &mut data);
        match (status, symbol, data) {
            (RETURN, ..) => Ok(result),
            (SIGNAL, symbol, data) => {
                self.non_local_exit_clear();
                Err(ErrorKind::Signal {
                    symbol: unsafe { TempValue::new(symbol.assume_init()) },
                    data: unsafe { TempValue::new(data.assume_init()) },
                }
                .into())
            }
            (THROW, tag, value) => {
                self.non_local_exit_clear();
                Err(ErrorKind::Throw {
                    tag: unsafe { TempValue::new(tag.assume_init()) },
                    value: unsafe { TempValue::new(value.assume_init()) },
                }
                .into())
            }
            _ => panic!("Unexpected non local exit status {}", status),
        }
    }

    /// Converts a Rust's `Result` to either a normal value, or a non-local exit in Lisp.
    #[inline]
    pub(crate) unsafe fn maybe_exit(&self, result: Result<Value<'_>>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(error) => match error.downcast_ref::<ErrorKind>() {
                Some(err) => self.handle_known(err),
                _ => self
                    .signal_str(ERROR, &format!("{}", error))
                    .unwrap_or_else(|_| panic!("Failed to signal {}", error)),
            },
        }
    }

    #[inline]
    pub(crate) fn handle_panic(&self, result: thread::Result<emacs_value>) -> emacs_value {
        match result {
            Ok(v) => v,
            Err(error) => {
                // TODO: Try to check for some common types to display?
                let mut m: result::Result<String, Box<dyn Any>> = Err(error);
                if let Err(error) = m {
                    m = error.downcast::<String>().map(|v| *v);
                }
                if let Err(error) = m {
                    m = match error.downcast::<ErrorKind>() {
                        // TODO: Explain safety.
                        Ok(err) => unsafe { return self.handle_known(&*err) },
                        Err(error) => Err(error),
                    }
                }
                if let Err(error) = m {
                    m = Ok(format!("{:#?}", error));
                }
                self.signal_str(PANIC, &m.expect("Logic error")).expect("Fail to signal panic")
            }
        }
    }

    pub(crate) fn define_errors(&self) -> Result<()> {
        // FIX: Make panics louder than errors, by somehow make sure that 'rust-panic is
        // not a sub-type of 'error.
        self.define_error(PANIC, "Rust panic", &["error"])?;
        self.define_error(ERROR, "Rust error", &["error"])?;
        self.define_error(
            WRONG_TYPE_USER_PTR,
            "Wrong type user-ptr",
            &[ERROR, "wrong-type-argument"],
        )?;
        Ok(())
    }

    unsafe fn handle_known(&self, err: &ErrorKind) -> emacs_value {
        match err {
            ErrorKind::Signal { symbol, data } => self.signal(symbol.raw, data.raw),
            ErrorKind::Throw { tag, value } => self.throw(tag.raw, value.raw),
            ErrorKind::WrongTypeUserPtr { .. } => self
                .signal_str(WRONG_TYPE_USER_PTR, &format!("{}", err))
                .unwrap_or_else(|_| panic!("Failed to signal {}", err)),
        }
    }

    // TODO: Prepare static values for the symbols.
    fn signal_str(&self, symbol: &str, message: &str) -> Result<emacs_value> {
        let message = message.into_lisp(&self)?;
        let data = self.list([message])?;
        let symbol = self.intern(symbol)?;
        unsafe { Ok(self.signal(symbol.raw, data.raw)) }
    }

    fn define_error(&self, name: &str, message: &str, parents: &[&str]) -> Result<Value<'_>> {
        // We can't use self.list here, because subr::list is not yet initialized.
        let parent_symbols = self.call(
            "list",
            &parents.iter().map(|p| self.intern(p)).collect::<Result<Vec<Value>>>()?,
        )?;
        self.call("define-error", (self.intern(name)?, message, parent_symbols))
    }

    pub(crate) fn non_local_exit_get(
        &self,
        symbol: &mut MaybeUninit<emacs_value>,
        data: &mut MaybeUninit<emacs_value>,
    ) -> emacs_funcall_exit {
        // Safety: The C code writes to these pointers. It doesn't read from them.
        unsafe_raw_call_no_exit!(self, non_local_exit_get, symbol.as_mut_ptr(), data.as_mut_ptr())
    }

    pub(crate) fn non_local_exit_clear(&self) {
        unsafe_raw_call_no_exit!(self, non_local_exit_clear)
    }

    /// # Safety
    ///
    /// The given raw values must still live.
    #[allow(unused_unsafe)]
    pub(crate) unsafe fn throw(&self, tag: emacs_value, value: emacs_value) -> emacs_value {
        unsafe_raw_call_no_exit!(self, non_local_exit_throw, tag, value);
        tag
    }

    /// # Safety
    ///
    /// The given raw values must still live.
    #[allow(unused_unsafe)]
    pub(crate) unsafe fn signal(&self, symbol: emacs_value, data: emacs_value) -> emacs_value {
        unsafe_raw_call_no_exit!(self, non_local_exit_signal, symbol, data);
        symbol
    }
}

/// Emacs-specific extension methods for [`Result`].
///
/// [`Result`]: type.Result.html
pub trait ResultExt<T, E> {
    /// Unwraps a result, yielding the content of an [`Ok`].
    ///
    /// # Panics
    ///
    /// Panics if the value is an [`Err`], using a sensible panic value.
    ///
    /// If the underlying error is an [`ErrorKind`], it will be used as the value of the panic,
    /// which makes the `#[defun]` behave as if the corresponding non-local exit was propagated.
    /// Otherwise, tries to use [`Display`] to get a descriptive error message.
    ///
    /// This is useful when errors cannot be propagated using [`Result`], e.g. callbacks whose types
    /// are dictated by 3rd-party libraries.
    ///
    /// # Safety
    ///
    /// The panic must not propagate across an FFI boundary, e.g. this must not be used in callbacks
    /// that will be called by C code. See Rust's [`issue #52652`].
    ///
    /// [`Ok`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Ok
    /// [`Err`]: https://doc.rust-lang.org/std/result/enum.Result.html#variant.Err
    /// [`ErrorKind`]: enum.ErrorKind.html
    /// [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html
    /// [`Result`]: type.Result.html
    /// [`issue #52652`]: https://github.com/rust-lang/rust/issues/52652
    #[deprecated(since = "0.12.0", note = "Use Result or a variable to track error instead")]
    unsafe fn unwrap_or_propagate(self) -> T;
}

impl<T> ResultExt<T, Error> for Result<T> {
    #[inline]
    unsafe fn unwrap_or_propagate(self) -> T {
        self.unwrap_or_else(|error| {
            match error.downcast::<ErrorKind>() {
                Ok(err) => panic!(err),
                Err(error) => panic!("{}", error),
            };
        })
    }
}
