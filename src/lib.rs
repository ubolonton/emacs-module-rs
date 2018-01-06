extern crate libc;
extern crate emacs_module;

use std::ffi::CString;
use std::ptr;
use libc::ptrdiff_t;
use emacs_module::{emacs_runtime, emacs_env};
use self::error::HandleExit;

#[macro_use]
pub mod func;
pub mod error;
pub mod raw;

pub use emacs_module::{Dtor, EmacsVal, EmacsSubr};
pub use self::error::{Result, Error, ErrorKind};
pub use self::func::HandleFunc;

pub struct Env {
    pub(crate) raw: *mut emacs_env
}

// TODO: CloneToEmacs?
pub trait ToEmacs {
    fn to_emacs(&self, env: &Env) -> Result<EmacsVal>;
}

// TODO: CloneFromEmacs?
pub trait FromEmacs: Sized {
    fn from_emacs(env: &Env, value: EmacsVal) -> Result<Self>;
}

// TODO: Is it ok to do a blanket implementation so almost anything can be transferred to Emacs?
/// Used to allow a type to be exposed to Emacs Lisp, where its values appear as opaque objects, or
/// "embedded user pointers" (`#<user-ptr ...>`).
///
/// When a (boxed) value of this type is transferred to Emacs Lisp, the GC becomes its owner.
/// Afterwards, module code can only access it through references.
pub trait Transfer: Sized {
    /// Finalizes a value. This is called by the GC when it discards a value of this type. Module
    /// code that needs custom destructor logic should implement [`Drop`], instead of overriding
    /// this.
    ///
    /// This function also serves as a form of runtime type tag.
    unsafe extern "C" fn finalizer(ptr: *mut libc::c_void) {
        #[cfg(build = "debug")]
        println!("Finalizing {} {:#?}", Self::type_name(), ptr);
        Box::from_raw(ptr as *mut Self);
    }

    /// Returns the name of this type. This is used to report runtime type error, when a function
    /// expects this type, but some Lisp code passes a different type of "user pointer".
    fn type_name() -> &'static str;

    // TODO: Consider using a wrapper struct to carry the type info, to enable better runtime
    // reporting of type error (and to enable something like `rs-module/type-of`).
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

impl From<*mut emacs_env> for Env {
    fn from(raw: *mut emacs_env) -> Env {
        Env { raw }
    }
}

impl From<*mut emacs_runtime> for Env {
    fn from(runtime: *mut emacs_runtime) -> Env {
        let raw = unsafe {
            let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
            get_env(runtime)
        };
        Env { raw }
    }
}

impl Env {
    pub fn raw(&self) -> *mut emacs_env {
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

    pub fn take<T: Transfer>(&mut self, container: Box<T>) -> Result<EmacsVal> {
        let raw = Box::into_raw(container);
        let ptr = raw as *mut libc::c_void;
        raw_call!(self, make_user_ptr, Some(T::finalizer), ptr)
    }

    pub fn lend<'e, T: Transfer>(&'e mut self, value: EmacsVal) -> Result<&'e mut T> {
        match raw_call!(self, get_user_finalizer, value)? {
            Some(fin) if fin == T::finalizer => {
                let ptr = raw_call!(self, get_user_ptr, value)?;
                let raw = ptr as *mut T;
                unsafe { Ok(&mut *raw) }
            },
            Some(_) => {
                let expected = T::type_name();
                Err(ErrorKind::UserPtrHasWrongType { expected }.into())
            },
            None => {
                let expected = T::type_name();
                Err(ErrorKind::UnknownUserPtr { expected }.into())
            }
        }
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
