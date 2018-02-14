extern crate libc;
extern crate emacs_module;

use std::borrow::Borrow;
use std::cell::RefCell;
use std::sync::{Mutex, RwLock};
use std::ffi::CString;
use std::ptr;
use libc::ptrdiff_t;
use emacs_module::{emacs_runtime, emacs_env, emacs_value};
use self::error::HandleExit;

#[macro_use]
mod macros;

pub mod func;
pub mod error;
pub mod raw;

pub use emacs_module::EmacsSubr;
pub use self::error::{Result, Error, ErrorKind};
pub use self::func::HandleFunc;

#[repr(C)]
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
}

/// This is similar to an `RC`. TODO: Document better.
///
/// We don't need a custom `Clone` implementation that does ref counting. TODO: Explain
/// why (e.g. GC still keeps a ref during value's lifetime (does it?), get_mut() is always
/// unsafe...)
///
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Value<'e> {
    pub(crate) raw: emacs_value,
    pub(crate) env: &'e Env,
}

// CloneToLisp
pub trait ToLisp {
    fn to_lisp<'e>(&self, env: &'e Env) -> Result<Value<'e>>;
}

// CloneFromLisp
pub trait FromLisp: Sized {
    fn from_lisp(value: &Value) -> Result<Self>;
}

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

    // TODO: This should be derived automatically. Use `typename` crate or something.
    /// Returns the name of this type. This is used to report runtime type error, when a function
    /// expects this type, but some Lisp code passes a different type of "user pointer".
    fn type_name() -> &'static str;

    // TODO: Consider using a wrapper struct to carry the type info, to enable better runtime
    // reporting of type error (and to enable something like `rs-module/type-of`).
}

pub trait IntoLisp {
    fn into_lisp(self, env: &Env) -> Result<Value>;
}

pub type Finalizer = unsafe extern "C" fn(ptr: *mut libc::c_void);

/// Public APIs.
impl Env {
    pub fn raw(&self) -> *mut emacs_env {
        self.raw
    }

    pub fn intern(&self, name: &str) -> Result<Value> {
        raw_call_value!(self, intern, CString::new(name)?.as_ptr())
    }

    // TODO: Return an enum?
    pub fn type_of(&self, value: Value) -> Result<Value> {
        raw_call_value!(self, type_of, value.raw)
    }

    // TODO: Add a convenient macro?
    pub fn call(&self, name: &str, args: &[Value]) -> Result<Value> {
        let symbol = self.intern(name)?;
        // XXX Hmm
        let mut args: Vec<emacs_value> = args.iter().map(|v| v.raw).collect();
        raw_call_value!(self, funcall, symbol.raw, args.len() as ptrdiff_t, args.as_mut_ptr())
    }

    pub fn clone_to_lisp<T, U>(&self, value: U) -> Result<Value> where T: ToLisp, U: Borrow<T> {
        value.borrow().to_lisp(self)
    }

    pub fn move_to_lisp<T>(&self, value: T) -> Result<Value> where T: IntoLisp {
        value.into_lisp(self)
    }

    pub fn is_not_nil(&self, value: Value) -> Result<bool> {
        raw_call!(self, is_not_nil, value.raw)
    }

    pub fn eq(&self, a: Value, b: Value) -> Result<bool> {
        raw_call!(self, eq, a.raw, b.raw)
    }

    pub fn list(&self, args: &[Value]) -> Result<Value> {
        self.call("list", args)
    }

    pub fn provide(&self, name: &str) -> Result<Value> {
        let name = self.intern(name)?;
        call_lisp!(self, "provide", name)
    }

    pub fn message(&self, text: &str) -> Result<Value> {
        let text = text.to_lisp(self)?;
        call_lisp!(self, "message", text)
    }
}

/// Implementation details.
impl Env {
    fn strip_trailing_zero_bytes(bytes: &mut Vec<u8>) {
        let mut len = bytes.len();
        while len > 0 && bytes[len - 1] == 0 {
            bytes.pop(); // strip trailing 0-byte(s)
            len -= 1;
        }
    }

    fn string_bytes(&self, value: &Value) -> Result<Vec<u8>> {
        let mut len: isize = 0;
        let mut bytes = unsafe {
            let copy_string_contents = raw_fn!(self, copy_string_contents)?;
            let ok: bool = self.handle_exit(copy_string_contents(
                self.raw, value.raw, ptr::null_mut(), &mut len))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool. TODO: Use a custom error type instead of panicking here.
            if !ok {
                panic!("Emacs failed to give string's length but did not raise a signal");
            }

            let mut bytes = vec![0u8; len as usize];
            let ok: bool = self.handle_exit(copy_string_contents(
                self.raw, value.raw, bytes.as_mut_ptr() as *mut i8, &mut len))?;
            // Technically this shouldn't happen, and the return type of copy_string_contents
            // should be void, not bool. TODO: Use a custom error type instead of panicking here.
            if !ok {
                panic!("Emacs failed to copy string but did not raise a signal");
            }
            bytes
        };
        Self::strip_trailing_zero_bytes(&mut bytes);
        Ok(bytes)
    }

    fn get_raw_pointer<T: Transfer>(&self, value: emacs_value) -> Result<*mut T> {
        match raw_call!(self, get_user_finalizer, value)? {
            Some::<Finalizer>(fin) if fin == T::finalizer => {
                let ptr: *mut libc::c_void = raw_call!(self, get_user_ptr, value)?;
                Ok(ptr as *mut T)
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

impl<'e> Value<'e> {
    pub fn new(raw: emacs_value, env: &'e Env) -> Self {
        Self { raw, env }
    }

    pub fn to_rust<T: FromLisp>(&self) -> Result<T> {
        FromLisp::from_lisp(self)
    }

    /// Returns a reference to the underlying Rust data wrapped by this value.
    pub fn get_ref<T: Transfer>(&self) -> Result<&T> {
        self.env.get_raw_pointer(self.raw).map(|r| unsafe {
            &*r
        })
    }

    /// Returns a mutable reference to the underlying Rust data wrapped by this value.
    ///
    /// # Safety
    ///
    /// There are several ways this can go wrong:
    /// - Lisp code can pass the same object through 2 different values in an argument list.
    /// - Rust code earlier in the call chain may have copied this value.
    /// - Rust code later in the call chain may receive a copy of this value.
    ///
    /// In general, it is better to wrap Rust data in `RefCell`, `Mutex`, or `RwLock`
    /// guards, before moving them to Lisp, and then only access them through these guards
    /// (which can be obtained back through `Value::get_ref()`.
    pub unsafe fn get_mut<T: Transfer>(&mut self) -> Result<&mut T> {
        self.env.get_raw_pointer(self.raw).map(|r| {
            &mut *r
        })
    }
}

impl ToLisp for i64 {
    fn to_lisp<'e>(&self, env: &'e Env) -> Result<Value<'e>> {
        raw_call_value!(env, make_integer, *self)
    }
}

// TODO: Make this more elegant. Can't implement it for trait bound Into<Vec<u8>>, since that would
// complain about conflicting implementations for i64.
impl ToLisp for str {
    fn to_lisp<'e>(&self, env: &'e Env) -> Result<Value<'e>> {
        let cstring = CString::new(self)?;
        let ptr = cstring.as_ptr();
        raw_call_value!(env, make_string, ptr, libc::strlen(ptr) as ptrdiff_t)
    }
}

impl FromLisp for i64 {
    fn from_lisp(value: &Value) -> Result<Self> {
        raw_call!(value.env, extract_integer, value.raw)
    }
}

impl FromLisp for String {
    // TODO: Optimize this.
    fn from_lisp(value: &Value) -> Result<Self> {
        let bytes = value.env.string_bytes(value)?;
        // FIX
        Ok(String::from_utf8(bytes).unwrap())
    }
}

impl<T: Transfer> IntoLisp for Box<T> {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        let raw = Box::into_raw(self);
        let ptr = raw as *mut libc::c_void;
        raw_call_value!(env, make_user_ptr, Some(T::finalizer), ptr)
    }
}

enable_transfers! {
    RefCell;
    Mutex;
    RwLock;
}
