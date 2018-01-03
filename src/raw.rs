//! This exposes some raw types for module to use (e.g. in `emacs_module_init`) without having to
//! declare the raw `emacs_module` as a dependency.
pub use emacs_module::{emacs_runtime, emacs_env};
