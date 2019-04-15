//! This provides a high-level binding to `emacs-module`, Emacs's support for dynamic modules.
//!
//! Code for a minimal module looks like this:
//!
//! ```no_run
//! use emacs::{defun, Env, Result, Value};
//!
//! emacs::plugin_is_GPL_compatible!();
//!
//! #[emacs::module(name = "greeting")]
//! fn init(_: &Env) -> Result<()> { Ok(()) }
//!
//! #[defun]
//! fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
//!     env.message(&format!("Hello, {}!", name))
//! }
//! ```
//!
//! ``` emacs-lisp
//! (require 'greeting)
//! (greeting-say-hello "Emacs")
//! ```
//!
//! See [User Guide] and [examples].
//!
//! [User Guide]: https://ubolonton.github.io/emacs-module-rs/
//! [Examples]: https://github.com/ubolonton/emacs-rs-examples/

use std::cell::RefCell;
use std::ffi::CString;

use failure;
use libc;

#[doc(inline)]
pub use emacs_macros::{defun, module};
use raw::*;

#[doc(no_inline)]
pub use failure::{Error, ResultExt};

#[doc(inline)]
pub use self::error::{ErrorKind, Result};

#[macro_use]
mod macros;
mod convert;

#[doc(hidden)]
pub mod error;

#[doc(hidden)]
pub mod func;

#[doc(hidden)]
pub mod globals;

// This exposes some raw types for module to use (e.g. in `emacs_module_init`) without having to
// declare the raw `emacs_module` as a dependency.
#[doc(hidden)]
pub mod raw {
    pub use emacs_module::{emacs_env, emacs_runtime, emacs_value};
}

// External dependencies that are mostly used by macros instead of user code.
#[doc(hidden)]
pub mod deps {
    pub use libc;
    pub use ctor;
    pub use lazy_static;
}

/// Main point of interaction with the Lisp runtime.
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
    /// Raw values "rooted" during the lifetime of this `Env`.
    pub(crate) protected: RefCell<Vec<emacs_value>>,
}

/// Like [`Env`], but is available only in exported functions. This has additional methods to handle
/// arguments passed from Lisp code.
///
/// [`Env`]: struct.Env.html
#[doc(hidden)]
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
/// [`Env`]: struct.Env.html
#[derive(Debug, Clone, Copy)]
pub struct Value<'e> {
    pub(crate) raw: emacs_value,
    pub env: &'e Env,
}

/// Converting Lisp [`Value`] into a Rust type.
///
/// # Implementation
///
/// The lifetime parameter is put on the trait itself, instead of the method. This allows it to be
/// implemented for [`Value`] itself.
///
/// [`Value`]: struct.Value.html
pub trait FromLisp<'e>: Sized {
    fn from_lisp(value: Value<'e>) -> Result<Self>;
}

/// Converting a Rust type into Lisp [`Value`].
///
/// # Implementation
///
/// The lifetime parameter is put on the trait itself, instead of the method. This allows the impl
/// for [`Value`] to simply return the input, instead of having to create a new [`Value`].
///
/// [`Value`]: struct.Value.html
pub trait IntoLisp<'e> {
    // TODO: Consider putting the lifetime parameter on the method. Look at rustler, maybe use its
    // env lifetime invariance trick.
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

/// Public APIs.
impl Env {
    #[doc(hidden)]
    pub unsafe fn new(raw: *mut emacs_env) -> Self {
        let protected = RefCell::new(vec![]);
        Self { raw, protected }
    }

    #[doc(hidden)]
    pub unsafe fn from_runtime(runtime: *mut emacs_runtime) -> Self {
        let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
        let raw = get_env(runtime);
        Self::new(raw)
    }

    #[doc(hidden)]
    pub fn raw(&self) -> *mut emacs_env {
        self.raw
    }

    pub fn intern(&self, name: &str) -> Result<Value<'_>> {
        raw_call_value!(self, intern, CString::new(name)?.as_ptr())
    }

    // TODO: Return an enum?
    pub fn type_of(&self, value: Value<'_>) -> Result<Value<'_>> {
        raw_call_value!(self, type_of, value.raw)
    }

    // TODO: Add a convenient macro?
    pub fn call(&self, name: &str, args: &[Value<'_>]) -> Result<Value<'_>> {
        let symbol = self.intern(name)?;
        // XXX Hmm
        let mut args: Vec<emacs_value> = args.iter().map(|v| v.raw).collect();
        raw_call_value!(self, funcall, symbol.raw, args.len() as libc::ptrdiff_t, args.as_mut_ptr())
    }

    // TODO: Add a method to Value instead.
    pub fn is_not_nil(&self, value: Value<'_>) -> bool {
        raw_call_no_exit!(self, is_not_nil, value.raw)
    }

    // TODO: Implement Eq for Value instead.
    pub fn eq(&self, a: Value<'_>, b: Value<'_>) -> bool {
        raw_call_no_exit!(self, eq, a.raw, b.raw)
    }

    pub fn list(&self, args: &[Value<'_>]) -> Result<Value<'_>> {
        self.call("list", args)
    }

    pub fn provide(&self, name: &str) -> Result<Value<'_>> {
        let name = self.intern(name)?;
        call_lisp!(self, "provide", name)
    }

    pub fn message(&self, text: &str) -> Result<Value<'_>> {
        let text = text.into_lisp(self)?;
        call_lisp!(self, "message", text)
    }
}

// TODO: Add tests to make sure the protected values are not leaked.
impl Drop for Env {
    fn drop(&mut self) {
        #[cfg(build = "debug")]
        println!("Unrooting {} values protected by {:?}", self.protected.borrow().len(), self);
        for raw in self.protected.borrow().iter() {
            raw_call_no_exit!(self, free_global_ref, *raw);
        }
    }
}

impl<'e> Value<'e> {
    /// Constructs a new `Value`. Module code should not call this directly. It is public only for
    /// some internal macros to use.
    ///
    /// # Safety
    ///
    /// The raw value must come from the given [`Env`].
    ///
    /// [`Env`]: struct.Env.html
    #[doc(hidden)]
    pub unsafe fn new(raw: emacs_value, env: &'e Env) -> Self {
        Self { raw, env }
    }

    /// Constructs a new `Value` and "roots" its underlying raw value (GC-managed) during the
    /// lifetime of the given [`Env`]. Module code should not call this directly. It is public only
    /// for some internal macros to use.
    ///
    /// # Safety
    ///
    /// The raw value must still be alive. This function is needed to protect new values returned
    /// from Emacs runtime, due to [this issue](https://github.com/ubolonton/emacs-module-rs/issues/2).
    ///
    /// [`Env`]: struct.Env.html
    #[allow(unused_unsafe)]
    #[doc(hidden)]
    pub unsafe fn new_protected(raw: emacs_value, env: &'e Env) -> Self {
        env.protected.borrow_mut().push(raw_call_no_exit!(env, make_global_ref, raw));
        Self::new(raw, env)
    }

    /// Converts this value into a Rust value of the given type.
    pub fn into_rust<T: FromLisp<'e>>(self) -> Result<T> {
        FromLisp::from_lisp(self)
    }

    // TODO: Rename this to `borrow_mut`? Also, remove FromLisp implementation for &T. On the other
    // hand, `Value` is similar to `Rc`, so `get_mut` may make sense.

    /// Returns a mutable reference to the underlying Rust data wrapped by this value.
    ///
    /// # Safety
    ///
    /// There are several ways this can go wrong:
    ///
    /// - Lisp code can pass the same object through 2 different values in an argument list.
    /// - Rust code earlier in the call chain may have copied this value.
    /// - Rust code later in the call chain may receive a copy of this value.
    ///
    /// In general, it is better to wrap Rust data in `RefCell`, `Mutex`, or `RwLock` guards, before
    /// moving them to Lisp, and then only access them through these guards (which can be obtained
    /// back through [`into_rust`]). This method is for squeezing out the last bit of performance in
    /// very rare situations.
    ///
    /// [`into_rust`]: #method.into_rust
    pub unsafe fn get_mut<T: Transfer>(&mut self) -> Result<&mut T> {
        self.env.get_raw_pointer(self.raw).map(|r| &mut *r)
    }
}
