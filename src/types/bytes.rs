use super::*;

/// A byte chunk that converts to and from a Lisp string as raw bytes, not as text.
///
/// Use it for binary data, or for text in an encoding other than UTF-8. Use [`String`] for text.
///
/// It is a wrapper, like [`serde_bytes`], because `u8` is already [`IntoLisp`]. A direct impl on
/// `Vec<u8>` would block a future generic `impl<T: IntoLisp> IntoLisp for Vec<T>`, which would turn
/// `Vec<u8>` into a Lisp vector of integers.
///
/// From Lisp, it takes the bytes of a unibyte string, or the UTF-8 encoding of a multibyte string.
/// To Lisp, it gives a unibyte string. This direction needs the `emacs-28` feature, because the
/// module API before Emacs 28 cannot make a unibyte string. See the [guide] for edge cases.
///
/// # Examples
///
/// ```
/// use emacs::{defun, Bytes, Result};
///
/// // In Lisp, (xor-bytes "\377\0" 1) returns "\376\1".
/// # #[cfg(feature = "emacs-28")]
/// #[defun]
/// fn xor_bytes(data: Bytes, key: u8) -> Result<Bytes> {
///     Ok(Bytes(data.0.iter().map(|b| b ^ key).collect()))
/// }
/// ```
///
/// [`serde_bytes`]: https://docs.rs/serde_bytes
/// [guide]: https://ubolonton.github.io/emacs-module-rs/latest/type-conversions.html#strings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Bytes<T = Vec<u8>>(pub T);

impl<T: From<Vec<u8>>> FromLisp<'_> for Bytes<T> {
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::String`](crate::LispType::String) | `rust-module-wrong-type` |
    /// | [`ModuleError::NonUnicodeString`](crate::ModuleError::NonUnicodeString) (Emacs 27+) | `rust-module-non-unicode-string` |
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        Ok(Bytes(value.clone_string_contents()?.into()))
    }
}

#[cfg(feature = "emacs-28")]
impl<T: AsRef<[u8]>> IntoLisp<'_> for Bytes<T> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let bytes = self.0.as_ref();
        let len = bytes.len();
        let ptr = bytes.as_ptr();
        // Safety: ptr and len are valid, coming from a slice.
        unsafe_raw_call_value!(env, make_unibyte_string, ptr.cast(), len as isize)
    }
}
