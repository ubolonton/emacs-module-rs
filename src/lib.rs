extern crate libc;
extern crate emacs_module;
extern crate failure;
#[macro_use]
extern crate failure_derive;

use std::ffi::CString;

use emacs_module::{emacs_runtime, emacs_env, emacs_value};
pub use self::error::{Result, Error};
// pub use self::error::NonLocal;

#[macro_use]
mod macros;

pub mod func;
pub mod error;
pub mod raw;
mod convert;

/// Main point of interaction with the Lisp runtime.
#[repr(C)]
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
}

/// Like [`Env`], but is available only in exported functions. This has additional methods to handle
/// arguments passed from Lisp code.
///
/// [`Env`]: struct.Env.html
#[derive(Debug)]
pub struct CallEnv {
    env: Env,
    nargs: usize,
    args: *mut emacs_value,
    data: *mut libc::c_void,
}

/// A type that represents Lisp values.
/// Values of this type can be copied around, but are lifetime-bound to the [`Env`] they come from.
///
/// They are also "proxy values" that are only useful when converted to Rust values, or used as
/// arguments when calling back into the Lisp runtime.
///
/// # Implementations
///
/// We don't need a custom `Clone` implementation that does ref counting. TODO: Explain
/// why (e.g. GC still keeps a ref during value's lifetime (does it?), get_mut() is always
/// unsafe...)
///
/// [`Env`]: struct.Env.html
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Value<'e> {
    pub(crate) raw: emacs_value,
    pub(crate) env: &'e Env,
}

#[derive(Debug)]
pub struct RootedValue {
    raw: emacs_value,
}

/// Converting Lisp [`Value`] into a Rust type.
///
/// [`Value`]: struct.Value.html
pub trait FromLisp: Sized {
    fn from_lisp(value: Value) -> Result<Self>;
}

/// Converting a Rust type into Lisp [`Value`].
///
/// # Implementations
///
/// The lifetime parameter is put on the trait itself, instead of the method. This allows the impl
/// for [`Value`] to simply return the input, instead of having to create a new [`Value`].
///
/// [`Value`]: struct.Value.html
pub trait IntoLisp<'e> {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>>;
}

/// Allowing a type to be exposed to Lisp, where its values appear as opaque objects, or "embedded
/// user pointers" (printed as `#<user-ptr ...>`).
///
/// When a (boxed) value of this type is transferred to Lisp, the GC becomes its owner. Afterwards,
/// module code can only access it through immutable references.
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

#[doc(hidden)]
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
        // TODO: Context. for NulError.
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

    pub fn is_not_nil(&self, value: Value) -> bool {
        raw_call_no_exit!(self, is_not_nil, value.raw)
    }

    pub fn eq(&self, a: Value, b: Value) -> bool {
        raw_call_no_exit!(self, eq, a.raw, b.raw)
    }

    pub fn list(&self, args: &[Value]) -> Result<Value> {
        self.call("list", args)
    }

    pub fn provide(&self, name: &str) -> Result<Value> {
        let name = self.intern(name)?;
        call_lisp!(self, "provide", name)
    }

    pub fn message(&self, text: &str) -> Result<Value> {
        let text = text.into_lisp(self)?;
        call_lisp!(self, "message", text)
    }
}

impl<'e> Value<'e> {
    /// Constructs a new `Value`
    ///
    /// # Safety
    ///
    /// The raw value must come from the given [`Env`].
    ///
    /// [`Env`]: struct.Env.html
    pub unsafe fn new(raw: emacs_value, env: &'e Env) -> Self {
        Self { raw, env }
    }

    /// Converts this value into a Rust value of the given type.
    pub fn into_rust<T: FromLisp>(self) -> Result<T> {
        FromLisp::from_lisp(self)
    }

    /// Returns a mutable reference to the underlying Rust data wrapped by this value.
    ///
    /// # Safety
    ///
    /// There are several ways this can go wrong:
    ///
    /// - Lisp code can pass the same object through 2 different values in an argument list.
    /// - Rust code earlier in the call chain may have cloned this value.
    /// - Rust code later in the call chain may receive a clone of this value.
    ///
    /// In general, it is better to wrap Rust data in `RefCell`, `Mutex`, or `RwLock`
    /// guards, before moving them to Lisp, and then only access them through these guards
    /// (which can be obtained back through [`into_rust`].
    ///
    /// [`into_rust`]: struct.Value.html#method.into_rust
    pub unsafe fn get_mut<T: Transfer>(&mut self) -> Result<&mut T> {
        self.env.get_raw_pointer(self.raw).map(|r| {
            &mut *r
        })
    }
}

impl RootedValue {
    /// # Safety
    ///
    /// The given raw value must still live.
    #[allow(unused_unsafe)]
    pub(crate) unsafe fn new(raw: emacs_value, env: &Env) -> Result<RootedValue> {
        let raw = raw_call!(env, make_global_ref, raw)?;
        Ok(RootedValue { raw })
    }

    // XXX: This should be in Drop implementation, but free_global_ref requires an env...
    // TODO: Add a Drop implementation that somehow schedule the free
    // to be called in a Lisp thread.
    pub(crate) fn uproot(self, env: &Env) -> Result<emacs_value> {
        let raw = self.raw;
        let _: () = raw_call!(env, free_global_ref, raw)?;
        Ok(raw)
    }
}

unsafe impl Send for RootedValue {}
unsafe impl Sync for RootedValue {}
