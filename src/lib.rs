extern crate libc;
extern crate emacs_module;

use std::borrow::Borrow;
use std::ffi::CString;

use emacs_module::{emacs_runtime, emacs_env, emacs_value};
pub use self::error::{Result, Error, ErrorKind};

#[macro_use]
mod macros;

pub mod func;
pub mod error;
pub mod raw;
mod convert;

#[repr(C)]
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
}

#[derive(Debug)]
pub struct CallEnv {
    env: Env,
    nargs: usize,
    args: *mut emacs_value,
    data: *mut libc::c_void,
}

/// This is similar to an `RC`. TODO: Document better.
///
/// We don't need a custom `Clone` implementation that does ref counting. TODO: Explain
/// why (e.g. GC still keeps a ref during value's lifetime (does it?), get_mut() is always
/// unsafe...)
///
/// This does not implement `Copy`, to statically catch some potential mistakes when
/// using e.g. `.get_mut()`. Use `.clone()` when necessary.
#[repr(C)]
#[derive(Debug, Clone)]
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

/// # Implementations
///
/// The lifetime parameter is put on the trait itself, instead of the method. This allows the impl
/// for `Value` to simply return the input, instead of having to create a new `Value`.
pub trait IntoLisp<'e> {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>>;
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

pub type Finalizer = unsafe extern "C" fn(ptr: *mut libc::c_void);

/// Public APIs.
impl Env {
    pub unsafe fn new(raw: *mut emacs_env) -> Self {
        Self { raw }
    }

    pub unsafe fn from_runtime(runtime: *mut emacs_runtime) -> Self {
        let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
        let raw = get_env(runtime);
        Self { raw }
    }

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
        raw_call_value!(self, funcall, symbol.raw, args.len() as libc::ptrdiff_t, args.as_mut_ptr())
    }

    pub fn clone_to_lisp<T, U>(&self, value: U) -> Result<Value> where T: ToLisp, U: Borrow<T> {
        value.borrow().to_lisp(self)
    }

    pub fn move_to_lisp<'e, T: IntoLisp<'e>>(&'e self, value: T) -> Result<Value> {
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

impl<'e> Value<'e> {
    pub unsafe fn new(raw: emacs_value, env: &'e Env) -> Self {
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
    /// - Rust code earlier in the call chain may have cloned this value.
    /// - Rust code later in the call chain may receive a clone of this value.
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
