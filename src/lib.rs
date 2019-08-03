//! This provides a high-level binding to `emacs-module`, Emacs's support for dynamic modules.
//!
//! Code for a minimal module looks like this:
//!
//! ```
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
//! ```emacs-lisp
//! (require 'greeting)
//! (greeting-say-hello "Emacs")
//! ```
//!
//! See [User Guide] and [examples].
//!
//! [User Guide]: https://ubolonton.github.io/emacs-module-rs/
//! [Examples]: https://github.com/ubolonton/emacs-rs-examples/

use std::convert::TryInto;

#[doc(inline)]
pub use emacs_macros::{defun, module};

#[doc(no_inline)]
pub use failure::{self, Error};

#[doc(inline)]
pub use self::{
    env::Env,
    value::Value,
    func::CallEnv,
    error::{ErrorKind, Result, ResultExt},
};

#[macro_use]
mod macros;
mod env;
mod value;
mod convert;

#[doc(hidden)]
pub mod error;

#[doc(hidden)]
pub mod func;

#[doc(hidden)]
pub mod init;

// This exposes some raw types for module to use (e.g. in `emacs_module_init`) without having to
// declare the raw `emacs_module` as a dependency.
#[doc(hidden)]
pub mod raw {
    pub use emacs_module::{emacs_env, emacs_runtime, emacs_value};
}

// External dependencies that are mostly used by macros instead of user code.
#[doc(hidden)]
pub mod deps {
    pub use ctor;
    pub use lazy_static;
}

// XXX: More accurate would be `CloneFromLisp` or `Decode`, but ...
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

// XXX: More accurate would be `CloneToLisp`, `Encode`, but ...
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
///
/// The 'static bound disallows transferring short-lived references, which can become invalid while
/// still being held by the Lisp runtime.
///
/// This works, because the returned string is copied into the Lisp runtime.
///
/// ```
/// use emacs::{defun, Result};
///
/// #[defun]
/// fn foo(s: &String) -> Result<&str> {
///     Ok(s)
/// }
/// ```
///
/// This doesn't work, because the function attempts to give the Lisp runtime a temporary reference.
///
/// ```compile_fail
/// use emacs::{defun, Result};
///
/// #[defun(user_ptr)]
/// fn foo(s: &String) -> Result<&str> {
///     Ok(s)
/// }
/// ```
pub trait Transfer: Sized + 'static {
    // TODO: This should be derived automatically. Use `typename` crate or something.
    /// Returns the name of this type. This is used to report runtime type error, when a function
    /// expects this type, but some Lisp code passes a different type of "user pointer".
    fn type_name() -> &'static str;

    // TODO: Consider using a wrapper struct to carry the type info, to enable better runtime
    // reporting of type error (and to enable something like `rs-module/type-of`).
}

/// A type that represents Lisp vectors. This is a newtype wrapper around [`Value`] that provides
/// vector-specific methods.
///
/// Arguments to #[[`defun`]] having this type will be type-checked. This type checking can be
/// omitted by manually wrapping a [`Value`]. Note that Emacs still does type checking when calling
/// methods on the vector.
///
/// ```
/// use emacs::{defun, Value, Vector, Result};
///
/// #[defun]
/// fn must_pass_vector(vector: Vector) -> Result<Vector> {
///     Ok(vector)
/// }
///
/// #[defun]
/// fn no_type_check(value: Value) -> Result<Vector> {
///     Ok(Vector(value))
/// }
/// ```
///
/// [`Value`]: struct.Value.html
/// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
#[derive(Debug, Clone, Copy)]
pub struct Vector<'e>(pub Value<'e>);

impl<'e> Vector<'e> {
    pub fn get<T: FromLisp<'e>>(&self, i: usize) -> Result<T> {
        let v = self.0;
        let env = v.env;
        raw_call_value!(env, vec_get, v.raw, i as isize)?.into_rust()
    }

    pub fn set<T: IntoLisp<'e>>(&self, i: usize, value: T) -> Result<()> {
        let v = self.0;
        let env = v.env;
        let value = value.into_lisp(env)?;
        raw_call!(env, vec_set, v.raw, i as isize, value.raw)
    }

    pub fn size(&self) -> Result<usize> {
        let v = self.0;
        let env = v.env;
        let result = raw_call_no_exit!(env, vec_size, v.raw).try_into().expect("invalid size from Emacs");
        env.handle_exit(result)
    }
}
