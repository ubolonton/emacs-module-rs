use std::{os, ptr, cmp};

use super::*;

impl FromLisp<'_> for String {
    #[cfg(not(feature = "utf-8-validation"))]
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        let bytes = value.env.string_bytes(value)?;
        // Safety: We trust Emacs to give us valid utf-8 bytes.
        unsafe { Ok(String::from_utf8_unchecked(bytes)) }
    }

    #[cfg(feature = "utf-8-validation")]
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        let bytes = value.env.string_bytes(value)?;
        Ok(String::from_utf8(bytes).unwrap())
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
        raw_call_value!(env, make_string, ptr as *const os::raw::c_char, len as isize)
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

impl<'e> Value<'e> {
    /// Copies the content of this Lisp string value to the given buffer as a null-terminated UTF-8
    /// string. Returns the copied bytes, excluding the null terminator.
    ///
    /// Signals an `args-out-of-range` error if the buffer is too small.
    pub fn copy_string_contents(self, buffer: &mut [u8]) -> Result<&[u8]> {
        let env = self.env;
        let ptr = buffer.as_mut_ptr() as *mut os::raw::c_char;
        let max_len = buffer.len();
        let mut len = max_len as isize;
        match raw_call!(env, copy_string_contents, self.raw, ptr, &mut len) {
            Ok(false) => panic!("Emacs failed to copy string but did not raise a signal"),
            Err(x) => Err(x),
            _ => {
                let n = cmp::min(max_len, len as usize) - 1;
                Ok(&buffer[0..n])
            }
        }
    }
}

impl Env {
    fn string_bytes(&self, value: Value<'_>) -> Result<Vec<u8>> {
        let mut len: isize = 0;
        let mut bytes = unsafe {
            let copy_string_contents = raw_fn!(self, copy_string_contents);
            let ok: bool = self.handle_exit(copy_string_contents(
                self.raw,
                value.raw,
                ptr::null_mut(),
                &mut len,
            ))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool.
            if !ok {
                panic!("Emacs failed to give string's length but did not raise a signal");
            }

            let mut bytes = vec![0u8; len as usize];
            let ok: bool = self.handle_exit(copy_string_contents(
                self.raw,
                value.raw,
                bytes.as_mut_ptr() as *mut os::raw::c_char,
                &mut len,
            ))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool.
            if !ok {
                panic!("Emacs failed to copy string but did not raise a signal");
            }
            bytes
        };
        strip_trailing_zero_bytes(&mut bytes);
        Ok(bytes)
    }
}

fn strip_trailing_zero_bytes(bytes: &mut Vec<u8>) {
    let mut len = bytes.len();
    while len > 0 && bytes[len - 1] == 0 {
        bytes.pop(); // strip trailing 0-byte(s)
        len -= 1;
    }
}
