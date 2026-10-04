use std::num::TryFromIntError;

use emacs_module::emacs_value;

use super::*;
use crate::{error::TempValue, Error, ErrorKind, ModuleError, RustError};

/// Normalizes the error of a narrowing `try_into` to [`TryFromIntError`], for [`out_of_range`].
/// `i64 -> i64` (used by `NonZeroI64`) goes through `std`'s reflexive `TryFrom<T> for T`, whose
/// error is [`Infallible`](std::convert::Infallible), not `TryFromIntError`.
trait IntoTryFromIntError {
    fn into_try_from_int_error(self) -> TryFromIntError;
}

impl IntoTryFromIntError for TryFromIntError {
    fn into_try_from_int_error(self) -> TryFromIntError {
        self
    }
}

impl IntoTryFromIntError for std::convert::Infallible {
    fn into_try_from_int_error(self) -> TryFromIntError {
        match self {}
    }
}

/// Returns [`RustError::IntegerOutOfRange`]. `value` is the Lisp value, if one exists.
fn out_of_range<E: IntoTryFromIntError>(value: Option<Value<'_>>, source: E) -> Error {
    ErrorKind::Rust(RustError::IntegerOutOfRange {
        value: value.map(TempValue::from_value),
        source: source.into_try_from_int_error(),
    })
    .into()
}

/// The call-site rule for `extract_integer` (Emacs 27+, bignums) and `make_integer` (Emacs 25,
/// 26, no bignums). `value` is the Lisp value, if one exists.
fn integer_out_of_range<'e>(
    value: Option<Value<'e>>,
) -> impl FnOnce(&Env, emacs_value) -> Option<ModuleError> + 'e {
    move |env, raw_symbol| {
        env.is_symbol(raw_symbol, symbol::overflow_error).then(|| {
            ModuleError::IntegerOutOfRange { value: value.map(TempValue::from_value) }
        })
    }
}

impl FromLisp<'_> for i64 {
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Integer`](crate::LispType::Integer) | `rust-module-wrong-type` |
    /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 27+) | `rust-module-integer-out-of-range` |
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        unsafe_raw_call!(value.env, extract_integer, value.raw; integer_out_of_range(Some(value)))
    }
}

macro_rules! int_from_lisp {
    ($name:ident) => {
        impl FromLisp<'_> for $name {
            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Integer`](crate::LispType::Integer) | `rust-module-wrong-type` |
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 27+) | `rust-module-integer-out-of-range` |
            /// | [`RustError::IntegerOutOfRange`](crate::RustError::IntegerOutOfRange) | `rust-integer-out-of-range` |
            #[cfg(not(feature = "lossy-integer-conversion"))]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i: i64 = value.into_rust()?;
                i.try_into().map_err(|source| out_of_range(Some(value), source))
            }

            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Integer`](crate::LispType::Integer) | `rust-module-wrong-type` |
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 27+) | `rust-module-integer-out-of-range` |
            #[cfg(feature = "lossy-integer-conversion")]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i: i64 = value.into_rust()?;
                Ok(i as $name)
            }
        }
    }
}

int_from_lisp!(i8);
int_from_lisp!(i16);
int_from_lisp!(i32);
int_from_lisp!(isize);

int_from_lisp!(u8);
int_from_lisp!(u16);
int_from_lisp!(u32);
int_from_lisp!(u64);
int_from_lisp!(usize);

// -------------------------------------------------------------------------------------------------

macro_rules! nonzero_int_from_lisp {
    ($name:ident($primitive:ident)) => {
        impl FromLisp<'_> for std::num::$name {
            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Integer`](crate::LispType::Integer) | `rust-module-wrong-type` |
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 27+) | `rust-module-integer-out-of-range` |
            /// | [`RustError::IntegerOutOfRange`](crate::RustError::IntegerOutOfRange) | `rust-integer-out-of-range` |
            #[cfg(not(feature = "lossy-integer-conversion"))]
            fn from_lisp(value: Value<'_>) -> Result<std::num::$name> {
                let i: i64 = value.into_rust()?;
                let i: $primitive =
                    i.try_into().map_err(|source| out_of_range(Some(value), source))?;
                i.try_into().map_err(|source| out_of_range(Some(value), source))
            }

            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Integer`](crate::LispType::Integer) | `rust-module-wrong-type` |
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 27+) | `rust-module-integer-out-of-range` |
            /// | [`RustError::IntegerOutOfRange`](crate::RustError::IntegerOutOfRange) | `rust-integer-out-of-range` |
            #[cfg(feature = "lossy-integer-conversion")]
            fn from_lisp(value: Value<'_>) -> Result<std::num::$name> {
                let i: i64 = value.into_rust()?;
                let i: $primitive = i as $primitive;
                i.try_into().map_err(|source| out_of_range(Some(value), source))
            }
        }
    }
}

nonzero_int_from_lisp!(NonZeroU8(u8));
nonzero_int_from_lisp!(NonZeroU16(u16));
nonzero_int_from_lisp!(NonZeroU32(u32));
nonzero_int_from_lisp!(NonZeroU64(u64));
nonzero_int_from_lisp!(NonZeroUsize(usize));

nonzero_int_from_lisp!(NonZeroI8(i8));
nonzero_int_from_lisp!(NonZeroI16(i16));
nonzero_int_from_lisp!(NonZeroI32(i32));
nonzero_int_from_lisp!(NonZeroI64(i64));
nonzero_int_from_lisp!(NonZeroIsize(isize));

// -------------------------------------------------------------------------------------------------

impl IntoLisp<'_> for i64 {
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 25, 26) | `rust-module-integer-out-of-range` |
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        unsafe_raw_call_value_unprotected!(env, make_integer, self; integer_out_of_range(None))
    }
}

macro_rules! int_into_lisp {
    ($name:ident) => {
        impl IntoLisp<'_> for $name {
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                (self as i64).into_lisp(env)
            }
        }
    };
    // Like the arm above, but for a type whose range can exceed the fixnum range on Emacs 25, 26.
    ($name:ident, overflow) => {
        impl IntoLisp<'_> for $name {
            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 25, 26) | `rust-module-integer-out-of-range` |
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                (self as i64).into_lisp(env)
            }
        }
    };
    ($name:ident, lossless) => {
        impl IntoLisp<'_> for $name {
            /// # Errors
            ///
            /// | Rust variant | Lisp signal, if the error propagates |
            /// |---|---|
            /// | [`ModuleError::IntegerOutOfRange`](crate::ModuleError::IntegerOutOfRange) (Emacs 25, 26) | `rust-module-integer-out-of-range` |
            /// | [`RustError::IntegerOutOfRange`](crate::RustError::IntegerOutOfRange) | `rust-integer-out-of-range` |
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                let i: i64 = self.try_into().map_err(|source| out_of_range(None, source))?;
                i.into_lisp(env)
            }
        }
    };
}

// Types where `as i64` is lossless.
int_into_lisp!(i8);
int_into_lisp!(i16);
int_into_lisp!(i32);
int_into_lisp!(u8);
int_into_lisp!(u16);
int_into_lisp!(u32);

// Types where `as i64` is lossy.
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(isize, overflow);
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(u64, overflow);
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(usize, overflow);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(isize, lossless);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(u64, lossless);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(usize, lossless);

// -------------------------------------------------------------------------------------------------

macro_rules! nonzero_int_into_lisp {
    ($name:ident) => {
        #[cfg(feature = "nonzero-integer-conversion")]
        impl IntoLisp<'_> for std::num::$name {
            /// # Errors
            ///
            /// Returns the errors of the [`IntoLisp`] impl of the primitive type.
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                self.get().into_lisp(env)
            }
        }
    };
}

nonzero_int_into_lisp!(NonZeroU8);
nonzero_int_into_lisp!(NonZeroU16);
nonzero_int_into_lisp!(NonZeroU32);
nonzero_int_into_lisp!(NonZeroU64);
nonzero_int_into_lisp!(NonZeroUsize);

nonzero_int_into_lisp!(NonZeroI8);
nonzero_int_into_lisp!(NonZeroI16);
nonzero_int_into_lisp!(NonZeroI32);
nonzero_int_into_lisp!(NonZeroI64);
nonzero_int_into_lisp!(NonZeroIsize);
