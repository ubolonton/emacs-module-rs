use std::{os, ptr, cmp};

use super::*;
use crate::{ModuleError, ErrorKind, RustError, error::TempValue};

impl FromLisp<'_> for String {
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::String`](crate::LispType::String) | `rust-module-wrong-type` |
    /// | [`ModuleError::NonUnicodeString`](crate::ModuleError::NonUnicodeString) (Emacs 27+) | `rust-module-non-unicode-string` |
    /// | [`RustError::InvalidUtf8`](crate::RustError::InvalidUtf8) | `rust-invalid-utf-8` |
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        let bytes = value.clone_string_contents()?;
        String::from_utf8(bytes).map_err(|source| {
            let value = TempValue::from_value(value);
            ErrorKind::Rust(RustError::InvalidUtf8 { value, source }).into()
        })
    }
}

// XXX: We don't unify impl for &str and impl for &String with an impl for Borrow<str>, because that
// would cause potential cause conflicts later on for other interesting &T. Check this again once
// specialization lands. https://github.com/rust-lang/rust/issues/31844
impl IntoLisp<'_> for &str {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let bytes = self.as_bytes();
        let len = bytes.len();
        let ptr = bytes.as_ptr();
        // Safety: ptr and len are valid, coming from a slice.
        unsafe_raw_call_value!(env, make_string, ptr as *const os::raw::c_char, len as isize)
    }
}

impl IntoLisp<'_> for &String {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        self.as_str().into_lisp(env)
    }
}

impl IntoLisp<'_> for String {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        self.as_str().into_lisp(env)
    }
}

// These byte chunk impls block a generic `Vec<T>`/`&[T]` conversion to and from a Lisp vector or
// list, because `u8` is already `FromLisp` and `IntoLisp`. We don't want those anyway, as they
// would be an expensive abstraction, on top of the vector vs. list ambiguity.
impl FromLisp<'_> for Vec<u8> {
    /// Gives the raw bytes of a Lisp string, without UTF-8 validation.
    ///
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::String`](crate::LispType::String) | `rust-module-wrong-type` |
    /// | [`ModuleError::NonUnicodeString`](crate::ModuleError::NonUnicodeString) (Emacs 27+) | `rust-module-non-unicode-string` |
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        value.clone_string_contents()
    }
}

impl FromLisp<'_> for Box<[u8]> {
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        Vec::<u8>::from_lisp(value).map(Vec::into_boxed_slice)
    }
}

#[cfg(feature = "emacs-28")]
impl IntoLisp<'_> for &[u8] {
    /// Gives a unibyte string. Needs the `emacs-28` feature, because the module API before Emacs 28
    /// cannot make a unibyte string.
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let len = self.len();
        let ptr = self.as_ptr();
        // Safety: ptr and len are valid, coming from a slice.
        unsafe_raw_call_value!(env, make_unibyte_string, ptr.cast(), len as isize)
    }
}

#[cfg(feature = "emacs-28")]
impl IntoLisp<'_> for &Vec<u8> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        self.as_slice().into_lisp(env)
    }
}

#[cfg(feature = "emacs-28")]
impl IntoLisp<'_> for Vec<u8> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        self.as_slice().into_lisp(env)
    }
}

#[cfg(feature = "emacs-28")]
impl IntoLisp<'_> for Box<[u8]> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        (*self).into_lisp(env)
    }
}

impl<'e> Value<'e> {
    /// Copies the content of this Lisp string value to the given buffer as a null-terminated UTF-8
    /// string. Returns the copied bytes, excluding the null terminator.
    ///
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::String`](crate::LispType::String) | `rust-module-wrong-type` |
    /// | [`ModuleError::NonUnicodeString`](crate::ModuleError::NonUnicodeString) (Emacs 27+) | `rust-module-non-unicode-string` |
    /// | [`ModuleError::BufferTooSmall`](crate::ModuleError::BufferTooSmall) | `rust-module-buffer-too-small` |
    pub fn copy_string_contents(self, buffer: &mut [u8]) -> Result<&[u8]> {
        let env = self.env;
        let ptr = buffer.as_mut_ptr() as *mut os::raw::c_char;
        let max_len = buffer.len();
        let mut len = max_len as isize;
        // Safety: ptr and len are valid, coming from a slice.
        // Emacs writes the required size to `len` before it signals a too-small buffer. This is
        // true on all versions, but the signal symbol and data differ between versions.
        let result = unsafe_raw_call!(env, copy_string_contents, self.raw, ptr, &mut len;
        |_, _| (len as usize > max_len).then(|| ModuleError::BufferTooSmall {
            actual: max_len,
            required: len as usize,
        }));
        match result {
            Ok(false) => unreachable!("Emacs failed to copy string but did not raise a signal"),
            Err(x) => Err(x),
            _ => {
                let n = cmp::min(max_len, len as usize) - 1;
                Ok(&buffer[0..n])
            }
        }
    }

    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::String`](crate::LispType::String) | `rust-module-wrong-type` |
    /// | [`ModuleError::NonUnicodeString`](crate::ModuleError::NonUnicodeString) (Emacs 27+) | `rust-module-non-unicode-string` |
    #[inline]
    pub fn clone_string_contents(self) -> Result<Vec<u8>> {
        self.env.clone_string_contents(self)
    }
}

impl Env {
    fn clone_string_contents(&self, value: Value<'_>) -> Result<Vec<u8>> {
        let mut len: isize = 0;
        let mut bytes = unsafe {
            let copy_string_contents = raw_fn!(self, copy_string_contents);
            let ok: bool = self.handle_module_exit(copy_string_contents(
                self.raw,
                value.raw,
                ptr::null_mut(),
                &mut len,
            ), |_, _| None)?;
            if !ok {
                unreachable!("Emacs failed to give string's length but did not raise a signal");
            }

            let mut bytes = vec![0u8; len as usize];
            let ok: bool = self.handle_module_exit(copy_string_contents(
                self.raw,
                value.raw,
                bytes.as_mut_ptr() as *mut os::raw::c_char,
                &mut len,
            ), |_, _| None)?;
            if !ok {
                unreachable!("Emacs failed to copy string but did not raise a signal");
            }
            bytes
        };
        bytes.pop();
        Ok(bytes)
    }
}
