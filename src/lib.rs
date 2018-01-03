extern crate libc;
extern crate emacs_module;

use std::ffi::CString;
use std::ptr;
use libc::ptrdiff_t;
use error::HandleExit;

#[macro_use]
pub mod func;
pub mod error;

pub use emacs_module::{Dtor, EmacsEnv, EmacsRT, EmacsVal, EmacsSubr};
pub use error::{Result, Error};
pub use func::HandleFunc;

// TODO: CloneToEmacs?
pub trait ToEmacs {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal>;
}

// TODO: CloneFromEmacs?
pub trait FromEmacs: Sized {
    fn from_emacs(env: &Env, value: EmacsVal) -> Result<Self>;
}

pub struct Env {
    pub(crate) raw: *mut EmacsEnv
}

impl ToEmacs for i64 {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        raw_call!(env, make_integer, *self)
    }
}

// TODO: Make this more elegant. Can't implement it for trait bound Into<Vec<u8>>, since that would
// complain about conflicting implementations for i64.
impl ToEmacs for str {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        let cstring = CString::new(self)?;
        let ptr = cstring.as_ptr();
        raw_call!(env, make_string, ptr, libc::strlen(ptr) as ptrdiff_t)
    }
}

impl FromEmacs for i64 {
    fn from_emacs(env: &Env, value: EmacsVal) -> Result<Self> {
        raw_call!(env, extract_integer, value)
    }
}

fn strip_trailing_zero_bytes(bytes: &mut Vec<u8>) {
    let mut len = bytes.len();
    while len > 0 && bytes[len - 1] == 0 {
        bytes.pop(); // strip trailing 0-byte(s)
        len -= 1;
    }
}

impl FromEmacs for String {
    fn from_emacs(env: &Env, value: EmacsVal) -> Result<Self> {
        let bytes = env.string_bytes(value)?;
        // FIX
        Ok(String::from_utf8(bytes).unwrap())
    }
}

impl From<*mut EmacsEnv> for Env {
    fn from(raw: *mut EmacsEnv) -> Env {
        Env { raw }
    }
}

impl From<*mut EmacsRT> for Env {
    fn from(runtime: *mut EmacsRT) -> Env {
        let raw = unsafe {
            let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
            get_env(runtime)
        };
        Env { raw }
    }
}

impl Env {
    pub fn raw(&self) -> *mut EmacsEnv {
        self.raw
    }

    fn string_bytes(&self, value: EmacsVal) -> Result<Vec<u8>> {
        let mut len: isize = 0;
        let mut bytes = unsafe {
            let copy_string_contents = raw_fn!(self, copy_string_contents)?;
            let ok = self.handle_exit(copy_string_contents(
                self.raw, value, ptr::null_mut(), &mut len))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool. TODO: Use a custom error type instead of panicking here.
            if !ok {
                panic!("Emacs failed to give string's length but did not raise a signal");
            }

            let mut bytes = vec![0u8; len as usize];
            let ok = self.handle_exit(copy_string_contents(
                self.raw, value, bytes.as_mut_ptr() as *mut i8, &mut len))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool. TODO: Use a custom error type instead of panicking here.
            if !ok {
                panic!("Emacs failed to copy string but did not raise a signal");
            }
            bytes
        };
        strip_trailing_zero_bytes(&mut bytes);
        Ok(bytes)
    }

    pub fn intern(&mut self, name: &str) -> Result<EmacsVal> {
        raw_call!(self, intern, CString::new(name)?.as_ptr())
    }

    // TODO: Return an enum?
    pub fn type_of(&self, value: EmacsVal) -> Result<EmacsVal> {
        raw_call!(self, type_of, value)
    }

    // TODO: Add a convenient macro?
    pub fn call(&mut self, name: &str, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        let symbol = self.intern(name)?;
        raw_call!(self, funcall, symbol, args.len() as ptrdiff_t, args.as_mut_ptr())
    }

    pub fn to_emacs<T: ToEmacs>(&self, value: T) -> Result<EmacsVal> {
        value.to_emacs(self)
    }

    pub fn from_emacs<T: FromEmacs>(&self, value: EmacsVal) -> Result<T> {
        FromEmacs::from_emacs(self, value)
    }

    pub fn is_not_nil(&self, value: EmacsVal) -> Result<bool> {
        raw_call!(self, is_not_nil, value)
    }

    pub fn eq(&self, a: EmacsVal, b: EmacsVal) -> Result<bool> {
        raw_call!(self, eq, a, b)
    }

    // TODO: Add a private call_shared to allow certain built-in to use &self.
    pub fn list(&mut self, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        self.call("list", args)
    }

    pub fn provide(&mut self, name: &str) -> Result<EmacsVal> {
        let name = self.intern(name)?;
        self.call("provide", &mut [name])
    }

    pub fn message(&mut self, text: &str) -> Result<EmacsVal> {
        let text = text.to_emacs(self)?;
        self.call("message", &mut [text])
    }
}
