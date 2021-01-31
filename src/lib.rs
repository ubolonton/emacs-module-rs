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


#[doc(inline)]
pub use emacs_macros::{defun, module};

#[doc(inline)]
pub use self::{
    env::Env,
    value::Value,
    global::GlobalRef,
    types::{FromLisp, IntoLisp, Transfer, Vector},
    func::CallEnv,
    error::{ErrorKind, Result, ResultExt, Error},
};

#[macro_use] mod macros;

#[doc(hidden)]
pub mod init;
#[doc(hidden)]
pub mod func;

mod env;
mod value;
mod types;
mod error;
mod call;
mod global;
mod symbol;
mod subr;

/// This exposes some raw types for module to use (e.g. in `emacs_module_init`) without having to
/// declare the raw `emacs_module` as a dependency.
#[doc(hidden)]
pub mod raw {
    pub use emacs_module::{emacs_runtime, emacs_env, emacs_value};
}

/// External dependencies that are mostly used by macros instead of user code.
#[doc(hidden)]
pub mod deps {
    pub use ctor;
}
