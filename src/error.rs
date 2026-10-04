#[doc(no_inline)]
use std::{any::Any, fmt::Display, mem::MaybeUninit, result, thread};

pub use anyhow::{self, Error};
use thiserror::Error;

use emacs_module::*;

use crate::{
    Env, Value,
    GlobalRef,
    global::OnceGlobalRef,
    symbol::{self, IntoLispSymbol},
    subr,
    call::IntoLispArgs,
};

// We use const instead of enum, in case Emacs add more exit statuses in the future.
// See https://github.com/rust-lang/rust/issues/36927
pub(crate) const RETURN: emacs_funcall_exit = emacs_funcall_exit_return;
pub(crate) const SIGNAL: emacs_funcall_exit = emacs_funcall_exit_signal;
pub(crate) const THROW: emacs_funcall_exit = emacs_funcall_exit_throw;

#[derive(Debug)]
pub struct TempValue {
    raw: emacs_value,
}

/// Defines new error signals.
///
/// TODO: Document this properly.
///
/// This macro can be used only once per Rust `mod`.
#[macro_export]
macro_rules! define_errors {
    ($( $name:ident $message:literal $( ( $( $parent:ident )+ ) )? )*) => {
        $crate::global_refs! {__emrs_init_global_refs_to_error_symbols__(init_to_symbol) =>
            $( $name )*
        }

        #[$crate::deps::ctor::ctor(crate_path = $crate::deps::ctor)]
        fn __emrs_define_errors__() {
            $crate::init::__CUSTOM_ERRORS__.try_lock()
                .expect("Failed to acquire a write lock on the list of initializers for custom error signals")
                .push(::std::boxed::Box::new(|env| {
                    $(
                        env.define_error($name, $message, [
                            $(
                                $(
                                    env.intern($crate::deps::emacs_macros::lisp_name!($parent))?
                                ),+
                            )?
                        ])?;
                    )*
                    Ok(())
                }));
        }
    }
}

/// Errors that this crate reports. Each variant names the origin of the error.
///
/// This enum is exhaustive. Its variants are the origins, which are a closed set: Lisp code exits
/// only by signal or throw. New failures go into [`ModuleError`] and [`RustError`].
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

    /// The module layer (`emacs-module.c`) rejected the arguments of a module API call.
    #[error(transparent)]
    Module(#[from] ModuleError),

    /// Rust code in this crate rejected a value.
    #[error(transparent)]
    Rust(#[from] RustError),
}

/// Errors that the module layer (`emacs-module.c`) detects.
///
/// If Rust code lets such an error propagate, Lisp code sees a signal with two parents:
/// `rust-module-error`, and the standard signal for the same failure.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ModuleError {
    /// The value has the wrong Lisp type. Lisp signal: `rust-module-wrong-type`, with data
    /// `(PREDICATE VALUE)`.
    #[error("Wrong type argument: expected {expected:?}")]
    #[non_exhaustive]
    WrongType { expected: LispType, value: TempValue },

    /// The multibyte string has chars outside Unicode, so it has no UTF-8 encoding. Emacs 27+
    /// checks this. Lisp signal: `rust-module-non-unicode-string`, with data
    /// `(unicode-string-p VALUE)`.
    #[error("Not a Unicode string")]
    #[non_exhaustive]
    NonUnicodeString { value: TempValue },

    /// Emacs rejected bytes from Rust, because they are not valid UTF-8. Emacs 28+ checks this.
    /// Safe Rust code cannot cause it, because a `&str` is always valid UTF-8. Lisp signal:
    /// `rust-module-invalid-utf-8`, with data `(utf-8-string-p VALUE)`.
    #[error("Invalid UTF-8")]
    #[non_exhaustive]
    InvalidUtf8 { value: TempValue },

    /// The buffer for [`Value::copy_string_contents`] is too small. `required` includes the null
    /// terminator. Lisp signal: `rust-module-buffer-too-small`, with data `(ACTUAL REQUIRED)`.
    #[error("Buffer too small: {actual} bytes, {required} required")]
    #[non_exhaustive]
    BufferTooSmall { actual: usize, required: usize },

    /// The vector index is out of range. Lisp signal: `rust-module-index-out-of-range`, with data
    /// `(VECTOR INDEX)`.
    #[error("Index {index} out of range")]
    #[non_exhaustive]
    IndexOutOfRange { vector: TempValue, index: isize },

    /// The integer does not fit. Emacs 27+: a Lisp bignum does not fit in `i64` (`value` is the
    /// bignum). Emacs 25, 26: an `i64` does not fit in a fixnum (`value` is `None`, because no
    /// Lisp value exists). Lisp signal: `rust-module-integer-out-of-range`, with data `(VALUE)`,
    /// or no data.
    #[error("Integer out of range")]
    #[non_exhaustive]
    IntegerOutOfRange { value: Option<TempValue> },

    /// A module-layer signal with no typed variant. If it propagates, Lisp code sees it unchanged.
    #[error("Module-layer signal: symbol={symbol:?} data={data:?}")]
    #[non_exhaustive]
    Signal { symbol: TempValue, data: TempValue },
}

/// Errors that Rust code in this crate detects.
///
/// If Rust code lets such an error propagate, Lisp code sees a signal with two parents:
/// `rust-error`, and the standard signal for the same failure.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum RustError {
    /// The value is a `user-ptr`, but it holds another Rust type.
    ///
    /// Lisp signal: `rust-wrong-type-user-ptr`, with data `(EXPECTED VALUE)`. `EXPECTED` is the
    /// name of the expected Rust type.
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
    /// (unwrap (wrap-f 7)) ; *** Eval error ***  Wrong type user-ptr: "core::cell::RefCell<i64>", #<user-ptr …>
    /// ```
    #[error("expected: {expected}")]
    #[non_exhaustive]
    WrongTypeUserPtr { expected: &'static str, value: TempValue },
}

/// A Lisp type that the module layer checks. See [`ModuleError::WrongType`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum LispType {
    Integer,
    Float,
    String,
    Vector,
    UserPtr,
    Process,
    PipeProcess,
}

impl LispType {
    /// The predicate that Lisp signal data uses for this type.
    fn predicate(self) -> &'static OnceGlobalRef {
        match self {
            LispType::Integer => symbol::integerp,
            LispType::Float => symbol::floatp,
            LispType::String => symbol::stringp,
            LispType::Vector => symbol::vectorp,
            LispType::UserPtr => symbol::user_ptrp,
            LispType::Process => symbol::processp,
            LispType::PipeProcess => symbol::pipe_process_p,
        }
    }
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
    /// Keeps a Lisp value in an error. To read it back, use the `unsafe` method [`value`].
    ///
    /// [`value`]: TempValue::value
    pub(crate) fn from_value(value: Value<'_>) -> Self {
        Self { raw: value.raw }
    }

    /// # Safety
    ///
    /// This must only be used with the [`Env`] from which the error originated.
    ///
    /// [`Env`]: struct.Env.html
    pub unsafe fn value<'e>(&self, env: &'e Env) -> Value<'e> {
        // SAFETY: Caller guarantees env is the Env from which this error originated.
        unsafe { Value::new(self.raw, env) }.protect()
    }
}

// XXX: Technically these are unsound, but they are necessary to use the `Fail` trait. We ensure
// safety by marking TempValue methods as unsafe.
unsafe impl Send for TempValue {}

unsafe impl Sync for TempValue {}

/// A pending non-local exit, read and cleared by [`Env::take_exit`].
enum Exit {
    Signal { symbol: emacs_value, data: emacs_value },
    Throw { tag: emacs_value, value: emacs_value },
}

impl Exit {
    /// Converts a non-local exit from Lisp code into the matching [`ErrorKind`]. Used by
    /// [`Env::handle_lisp_exit`] for both arms, and by [`Env::handle_module_exit`] for the `Throw`
    /// arm: a throw never comes from the module layer, so it keeps this same conversion there too.
    fn into_error(self) -> ErrorKind {
        match self {
            Exit::Signal { symbol, data } => ErrorKind::Signal {
                symbol: TempValue { raw: symbol },
                data: TempValue { raw: data },
            },
            Exit::Throw { tag, value } => ErrorKind::Throw {
                tag: TempValue { raw: tag },
                value: TempValue { raw: value },
            },
        }
    }
}

impl Env {
    /// Reads and clears the pending non-local exit. Returns `None` if the last call returned
    /// normally.
    fn take_exit(&self) -> Option<Exit> {
        let mut first = MaybeUninit::uninit();
        let mut second = MaybeUninit::uninit();
        // TODO: Check whether calling non_local_exit_check first makes a difference in performance.
        let status = self.non_local_exit_get(&mut first, &mut second);
        // SAFETY: Emacs writes both values for the statuses SIGNAL and THROW.
        let exit = match status {
            RETURN => return None,
            SIGNAL => unsafe {
                Exit::Signal { symbol: first.assume_init(), data: second.assume_init() }
            },
            THROW => unsafe {
                Exit::Throw { tag: first.assume_init(), value: second.assume_init() }
            },
            _ => panic!("Unexpected non local exit status {}", status),
        };
        self.non_local_exit_clear();
        Some(exit)
    }

    /// Handles a possible non-local exit after `funcall`. The exit comes from Lisp code, so it
    /// stays [`ErrorKind::Signal`] or [`ErrorKind::Throw`].
    #[inline]
    pub(crate) fn handle_lisp_exit<T>(&self, result: T) -> Result<T> {
        match self.take_exit() {
            None => Ok(result),
            Some(exit) => Err(exit.into_error().into()),
        }
    }

    /// Handles a possible non-local exit after a module function other than `funcall`.
    ///
    /// A signal from such a function comes from the module layer. The call-site `rule` classifies
    /// it first. It gets the raw signal symbol. A signal that no rule classifies becomes
    /// [`ModuleError::Signal`]. See ADR 0002.
    #[inline]
    pub(crate) fn handle_module_exit<T, R>(&self, result: T, rule: R) -> Result<T>
    where
        R: FnOnce(&Env, emacs_value) -> Option<ModuleError>,
    {
        match self.take_exit() {
            None => Ok(result),
            Some(Exit::Signal { symbol, data }) => {
                let error = rule(self, symbol)
                    .or_else(|| self.classify_wrong_type(symbol, data))
                    .unwrap_or(ModuleError::Signal {
                        symbol: TempValue { raw: symbol },
                        data: TempValue { raw: data },
                    });
                Err(ErrorKind::Module(error).into())
            }
            // The module layer does not throw. Keep such an exit unchanged.
            Some(exit @ Exit::Throw { .. }) => Err(exit.into_error().into()),
        }
    }

    /// Handles a possible non-local exit after a module function other than `funcall`, with no
    /// call-site rule.
    #[inline]
    pub(crate) fn handle_exit<T>(&self, result: T) -> Result<T> {
        self.handle_module_exit(result, |_, _| None)
    }

    /// Returns whether `raw` is the symbol `symbol`. Call-site rules use this.
    pub(crate) fn is_symbol(&self, raw: emacs_value, symbol: &OnceGlobalRef) -> bool {
        // SAFETY: `raw` comes from the pending exit of this env, which is still live.
        (unsafe { Value::new(raw, self) }) == *symbol
    }

    /// The generic rule: classifies a `wrong-type-argument` signal from the module layer. Returns
    /// `None` for other symbols, and for predicates with no typed variant.
    ///
    /// The data of `wrong-type-argument` is `(PREDICATE VALUE)` on Emacs 25–32.
    fn classify_wrong_type(&self, symbol: emacs_value, data: emacs_value) -> Option<ModuleError> {
        if !self.is_symbol(symbol, symbol::wrong_type_argument) {
            return None;
        }
        // SAFETY: `data` comes from the pending exit of this env, which is still live. The calls
        // below run Lisp code, so protect `data` against GC bug 31238.
        let data = unsafe { Value::new(data, self) }.protect();
        // If these calls fail, the signal stays unclassified.
        let predicate = self.call(subr::car, [data]).ok()?;
        let value = TempValue::from_value(self.call(subr::cadr, [data]).ok()?);
        // Emacs 27's `extract_integer` signals `numberp`, not `integerp`, for a non-number.
        // Confirmed by running the integration tests on Emacs 25-32: 27 is the only version that
        // does this.
        let expected = if predicate == *symbol::integerp || predicate == *symbol::numberp {
            LispType::Integer
        } else if predicate == *symbol::floatp {
            LispType::Float
        } else if predicate == *symbol::stringp {
            LispType::String
        } else if predicate == *symbol::vectorp {
            LispType::Vector
        // Emacs 25 says `user-ptr`. Later versions say `user-ptrp`.
        } else if predicate == *symbol::user_ptrp || predicate == *symbol::user_ptr {
            LispType::UserPtr
        } else if predicate == *symbol::processp {
            LispType::Process
        } else if predicate == *symbol::pipe_process_p {
            LispType::PipeProcess
        } else if predicate == *symbol::unicode_string_p {
            return Some(ModuleError::NonUnicodeString { value });
        } else if predicate == *symbol::utf_8_string_p {
            return Some(ModuleError::InvalidUtf8 { value });
        } else {
            return None;
        };
        Some(ModuleError::WrongType { expected, value })
    }

    /// Converts a Rust's `Result` to either a normal value, or a non-local exit in Lisp.
    #[inline]
    pub(crate) unsafe fn maybe_exit(&self, result: Result<Value<'_>>) -> emacs_value {
        match result {
            Ok(v) => v.raw,
            Err(error) => match error.downcast_ref::<ErrorKind>() {
                Some(err) => {
                    // SAFETY: TempValue's raw values remain live for the duration of this error.
                    unsafe { self.handle_known(err) }
                }
                _ => self
                    .signal_internal(symbol::rust_error, &format!("{}", error))
                    .unwrap_or_else(|_| panic!("Failed to signal {}", error)),
            },
        }
    }

    /// Converts a caught unwinding panic into a non-local exit in Lisp.
    ///
    /// If there was no error, return the raw `emacs_value`.
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
                // TODO: Remove this when we remove `unwrap_or_propagate`.
                if let Err(error) = m {
                    m = match error.downcast::<ErrorKind>() {
                        // TODO: Explain safety.
                        Ok(err) => unsafe { return self.handle_known(&*err); },
                        Err(error) => Err(error),
                    }
                }
                if let Err(error) = m {
                    m = Ok(format!("{:#?}", error));
                }
                self.signal_internal(symbol::rust_panic, &m.expect("Logic error")).expect("Fail to signal panic")
            }
        }
    }

    pub(crate) fn define_core_errors(&self) -> Result<()> {
        // FIX: Make panics louder than errors, by somehow make sure that 'rust-panic is
        // not a sub-type of 'error.
        self.define_error(symbol::rust_panic, "Rust panic", (symbol::error, ))?;
        self.define_error(symbol::rust_error, "Rust error", (symbol::error, ))?;
        self.define_error(
            symbol::rust_wrong_type_user_ptr,
            "Wrong type user-ptr",
            (symbol::rust_error, symbol::wrong_type_argument),
        )?;
        // Module-layer errors. Each symbol uses the message of its standard parent, so printed
        // errors do not change. See ADR 0003.
        self.define_error(symbol::rust_module_error, "Emacs module error", (symbol::error, ))?;
        for name in [
            symbol::rust_module_wrong_type,
            symbol::rust_module_non_unicode_string,
            symbol::rust_module_invalid_utf_8,
        ] {
            self.define_error(
                name,
                "Wrong type argument",
                (symbol::rust_module_error, symbol::wrong_type_argument),
            )?;
        }
        // Emacs 31 signals `memory-buffer-too-small` for a too-small buffer. Earlier versions
        // signal `args-out-of-range`. Keep both parents where both exist, so old handlers work.
        let buffer_too_small_is_defined = self
            .call("get", (symbol::memory_buffer_too_small, self.intern("error-conditions")?))?
            .is_not_nil();
        if buffer_too_small_is_defined {
            self.define_error(
                symbol::rust_module_buffer_too_small,
                "Memory buffer too small",
                (
                    symbol::rust_module_error,
                    symbol::args_out_of_range,
                    symbol::memory_buffer_too_small,
                ),
            )?;
        } else {
            self.define_error(
                symbol::rust_module_buffer_too_small,
                "Memory buffer too small",
                (symbol::rust_module_error, symbol::args_out_of_range),
            )?;
        }
        self.define_error(
            symbol::rust_module_index_out_of_range,
            "Args out of range",
            (symbol::rust_module_error, symbol::args_out_of_range),
        )?;
        self.define_error(
            symbol::rust_module_integer_out_of_range,
            "Arithmetic overflow error",
            (symbol::rust_module_error, symbol::overflow_error),
        )?;
        Ok(())
    }

    /// Raises the Lisp signal or throw for `err`. See ADR 0003.
    ///
    /// # Safety
    ///
    /// The `TempValue`s in `err` must come from this env, and must still be live.
    unsafe fn handle_known(&self, err: &ErrorKind) -> emacs_value {
        // SAFETY: Guaranteed by the caller.
        let raised = match err {
            ErrorKind::Signal { symbol, data } => {
                return unsafe { self.non_local_exit_signal(symbol.raw, data.raw) };
            }
            ErrorKind::Throw { tag, value } => {
                return unsafe { self.non_local_exit_throw(tag.raw, value.raw) };
            }
            ErrorKind::Module(error) => unsafe { self.signal_module_error(error) },
            ErrorKind::Rust(error) => unsafe { self.signal_rust_error(error) },
        };
        raised.unwrap_or_else(|_| {
            self.signal_internal(symbol::rust_error, &format!("{}", err))
                .unwrap_or_else(|_| panic!("Failed to signal {}", err))
        })
    }

    /// Raises the signal for a module-layer error.
    ///
    /// # Safety
    ///
    /// Same as [`Env::handle_known`].
    unsafe fn signal_module_error(&self, error: &ModuleError) -> Result<emacs_value> {
        match error {
            ModuleError::WrongType { expected, value } => {
                // SAFETY: Guaranteed by the caller.
                let value = unsafe { value.value(self) };
                self.signal_with(
                    symbol::rust_module_wrong_type,
                    self.list((expected.predicate(), value))?,
                )
            }
            ModuleError::NonUnicodeString { value } => {
                // SAFETY: Guaranteed by the caller.
                let value = unsafe { value.value(self) };
                self.signal_with(
                    symbol::rust_module_non_unicode_string,
                    self.list((symbol::unicode_string_p, value))?,
                )
            }
            ModuleError::InvalidUtf8 { value } => {
                // SAFETY: Guaranteed by the caller.
                let value = unsafe { value.value(self) };
                self.signal_with(
                    symbol::rust_module_invalid_utf_8,
                    self.list((symbol::utf_8_string_p, value))?,
                )
            }
            ModuleError::BufferTooSmall { actual, required } => self.signal_with(
                symbol::rust_module_buffer_too_small,
                self.list((*actual, *required))?,
            ),
            ModuleError::IndexOutOfRange { vector, index } => {
                // SAFETY: Guaranteed by the caller.
                let vector = unsafe { vector.value(self) };
                self.signal_with(
                    symbol::rust_module_index_out_of_range,
                    self.list((vector, *index))?,
                )
            }
            ModuleError::IntegerOutOfRange { value } => {
                let data = match value {
                    // SAFETY: Guaranteed by the caller.
                    Some(value) => self.list((unsafe { value.value(self) },))?,
                    None => symbol::nil.bind(self),
                };
                self.signal_with(symbol::rust_module_integer_out_of_range, data)
            }
            // Lisp code sees an unclassified signal unchanged.
            // SAFETY: Guaranteed by the caller.
            ModuleError::Signal { symbol, data } => {
                Ok(unsafe { self.non_local_exit_signal(symbol.raw, data.raw) })
            }
        }
    }

    /// Raises the signal for an error that Rust code in this crate detected.
    ///
    /// # Safety
    ///
    /// Same as [`Env::handle_known`].
    unsafe fn signal_rust_error(&self, error: &RustError) -> Result<emacs_value> {
        match error {
            RustError::WrongTypeUserPtr { expected, value } => {
                // SAFETY: Guaranteed by the caller.
                let value = unsafe { value.value(self) };
                self.signal_with(symbol::rust_wrong_type_user_ptr, self.list((*expected, value))?)
            }
        }
    }

    /// Raises `symbol`, with `data` as the signal data. `data` must be a list.
    fn signal_with(&self, symbol: &GlobalRef, data: Value<'_>) -> Result<emacs_value> {
        // SAFETY: `symbol` is a global reference, and `data` is bound to this env.
        unsafe { Ok(self.non_local_exit_signal(symbol.bind(self).raw, data.raw)) }
    }

    fn signal_internal(&self, symbol: &GlobalRef, message: &str) -> Result<emacs_value> {
        self.signal_with(symbol, self.list((message,))?)
    }

    /// Defines a new Lisp error signal. This is the equivalent of the Lisp function's [`define-error`].
    ///
    /// The error name can be either a string, a [`Value`], or a [`GlobalRef`].
    ///
    /// [`define-error`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Error-Symbols.html
    pub fn define_error<'e, N, P>(&'e self, name: N, message: &str, parents: P) -> Result<Value<'e>>
        where N: IntoLispSymbol<'e>, P: IntoLispArgs<'e> {
        self.call("define-error", (name.into_lisp_symbol(self)?, message, self.list(parents)?))
    }

    /// Signals a Lisp error. This is the equivalent of the Lisp function's [`signal`].
    ///
    /// [`signal`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Signaling-Errors.html#index-signal
    pub fn signal<'e, S, D, T>(&'e self, symbol: S, data: D) -> Result<T> where S: IntoLispSymbol<'e>, D: IntoLispArgs<'e> {
        let symbol = TempValue { raw: symbol.into_lisp_symbol(self)?.raw };
        let data = TempValue { raw: self.list(data)?.raw };
        Err(ErrorKind::Signal { symbol, data }.into())
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
    pub(crate) unsafe fn non_local_exit_throw(&self, tag: emacs_value, value: emacs_value) -> emacs_value {
        unsafe_raw_call_no_exit!(self, non_local_exit_throw, tag, value);
        tag
    }

    /// # Safety
    ///
    /// The given raw values must still live.
    #[allow(unused_unsafe)]
    pub(crate) unsafe fn non_local_exit_signal(&self, symbol: emacs_value, data: emacs_value) -> emacs_value {
        unsafe_raw_call_no_exit!(self, non_local_exit_signal, symbol, data);
        symbol
    }
}

/// Emacs-specific extension methods for the standard library's [`Result`].
///
/// [`Result`]: result::Result
pub trait ResultExt<T, E> {
    /// Converts the error into a Lisp signal if this result is an [`Err`]. The first element of the
    /// associated signal data will be a string formatted with [`Display::fmt`].
    ///
    /// If the result is an [`Ok`], it is returned unchanged.
    fn or_signal<'e, S>(self, env: &'e Env, symbol: S) -> Result<T> where S: IntoLispSymbol<'e>;
}

impl<T, E: Display> ResultExt<T, E> for result::Result<T, E> {
    fn or_signal<'e, S>(self, env: &'e Env, symbol: S) -> Result<T> where S: IntoLispSymbol<'e> {
        self.or_else(|err| env.signal(symbol, (
            format!("{}", err),
        )))
    }
}
