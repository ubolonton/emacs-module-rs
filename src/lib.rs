extern crate libc;
extern crate regex;

use std::ffi::CString;
use libc::ptrdiff_t;
use std::ptr;
use error::HandleExit;

mod emacs_gen;
#[macro_use]
pub mod func;
pub mod error;

pub use emacs_gen::{Dtor, EmacsEnv, EmacsRT, EmacsVal, EmacsSubr};
pub use error::{Result, Error};
pub use func::HandleFunc;

// TODO: How about IntoEmacs (which may include EmacsVal itself)?
pub trait ToEmacs {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal>;
}

pub trait IntoEmacs {
    fn into_emacs(self, env: &Env) -> Result<EmacsVal>;
}

// Technically this is CloneFromEmacs?
pub trait FromEmacs: Sized {
    fn from_emacs(env: &Env, value: EmacsVal) -> Result<Self>;
}

// TODO: BorrowFromEmacs?

// TODO: Do we need this? How about using an existing type, like Box<EmacsEnv>?
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
//        println!("to_emacs {}", self);
        // Rust string may fail to convert to CString. Raise non-local exit in that case.
        let cstring = env.to_cstring(self)?;
        let ptr = cstring.as_ptr();
        raw_call!(env, make_string, ptr, libc::strlen(ptr) as ptrdiff_t)
    }
}

impl ToEmacs for String {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        self.as_str().to_emacs(env)
    }
}

impl ToEmacs for [Box<ToEmacs>] {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal> {
        let args = &mut env.to_emacs_args(self)?;
        env.list(args)
    }
}

// XXX: Doesn't work, possibly because of blanket implementations of Into. See
// https://github.com/rust-lang/rust/issues/30191 and
// https://github.com/rust-lang/rust/issues/19032.
//impl <T> ToEmacs for T where Vec<u8>: From<T> {
//    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
//        unimplemented!()
//    }
//}

impl IntoEmacs for i64 {
    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
        raw_call!(env, make_integer, self)
    }
}

// XXX: Cannot do this. Box instances must contain sized types.
// Maybe related: https://github.com/rust-lang/rust/issues/27779.
//impl IntoEmacs for [Box<IntoEmacs>] {
//    fn into_emacs(self, env: &Env) -> Result<EmacsVal> {
//        unimplemented!()
//    }
//}

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

pub type Func = fn(env: &Env, args: &[EmacsVal], data: *mut libc::c_void) -> Result<EmacsVal>;

impl Env {
    pub fn raw(&self) -> *mut EmacsEnv {
        self.raw
    }

    pub fn to_cstring(&self, s: &str) -> Result<CString> {
        let cstring = CString::new(s)?;
        Ok(cstring)
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

            let mut bytes = Vec::<u8>::with_capacity(len as usize);
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

    // TODO: Return a Symbol.
    pub fn intern(&self, name: &str) -> Result<EmacsVal> {
        raw_call!(self, intern, self.to_cstring(name)?.as_ptr())
    }

    // TODO: Return a Symbol.
    pub fn type_of(&self, value: EmacsVal) -> Result<EmacsVal> {
        raw_call!(self, type_of, value)
    }

    // TODO: Should there be variants of this that deal with mixtures of types?
    pub fn call(&self, name: &str, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        let symbol = self.intern(name)?;
        raw_call!(self, funcall, symbol, args.len() as ptrdiff_t, args.as_mut_ptr())
    }

//    pub fn call1(&self, name: &str, args: &mut [Box<ToEmacs>]) -> Result<EmacsVal> {
//        self.call(name,&mut self.to_emacs_args(args)?)
//    }

    fn to_emacs_args(&self, args: &[Box<ToEmacs>]) -> Result<Vec<EmacsVal>> {
        let mut e_args: Vec<EmacsVal> = Vec::with_capacity(args.len());
        for value in args.iter() {
            e_args.push(value.to_emacs(self)?);
        }
        Ok(e_args)
    }

    pub fn to_emacs<T: ToEmacs>(&self, value: T) -> Result<EmacsVal> {
        value.to_emacs(self)
    }

    pub fn from_emacs<T: FromEmacs>(&self, value: EmacsVal) -> Result<T> {
        FromEmacs::from_emacs(&self, value)
    }

    pub fn is_not_nil(&self, value: EmacsVal) -> Result<bool> {
        raw_call!(self, is_not_nil, value)
    }

    pub fn eq(&self, a: EmacsVal, b: EmacsVal) -> Result<bool> {
        raw_call!(self, eq, a, b)
    }

    pub fn list(&self, args: &mut [EmacsVal]) -> Result<EmacsVal> {
        self.call("list", args)
    }

    pub fn provide(&self, name: &str) -> Result<EmacsVal> {
        self.call("provide", &mut [self.intern(name)?])
    }

    pub fn message(&self, text: &str) -> Result<EmacsVal> {
        self.call("message", &mut [
            text.to_emacs(self)?
        ])
    }
}
