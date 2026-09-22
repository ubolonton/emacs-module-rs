// When we use this crate's binary to generate the code to be checked into git, we don't want its
// lib code.
#![cfg(not(feature = "bindgen-code"))]
// These crate-wide attrs are useful because `bindgen`:

// generates types that don't and can't conform to the Rust naming conventions.
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// generates a lot of constants that aren't used in this crate directly
#![allow(dead_code)]

use std::{mem::size_of, os};

// build.rs emits `emacs_version = "N"` for the highest enabled version.
// When adding a new Emacs version N, add an entry here:
//   #[cfg(all(emacs_version = "N", not(feature = "bindgen-build")))]
//   include!("./emacs-module-N.rs");
// and an arm to EMACS_VERSION below.

#[cfg(all(emacs_version = "25", not(feature = "bindgen-build")))]
include!("./emacs-module.rs");

#[cfg(all(emacs_version = "28", not(feature = "bindgen-build")))]
include!("./emacs-module-28.rs");

#[cfg(feature = "bindgen-build")]
include!(concat!(env!("OUT_DIR"), "/emacs-module.rs"));

/// The Emacs version whose module ABI this build requires, i.e. the highest version among enabled
/// `emacs-N` crate features (or `"25"`, the baseline, if none are enabled).
#[cfg(emacs_version = "25")]
pub const EMACS_VERSION: &str = "25";
#[cfg(emacs_version = "28")]
pub const EMACS_VERSION: &str = "28";

/// The minimum `emacs_env::size` (in bytes) the running Emacs must report for this build's
/// enabled `emacs-N` feature(s) to be usable. `emacs_env`'s layout grows with each supported
/// version, and already resolves to the right one for the enabled features, so this doesn't need
/// a per-version arm like [`EMACS_VERSION`] does.
///
/// Callers compare this against the running Emacs's reported `emacs_env::size` at module load
/// time to detect whether it's new enough to provide the fields this build was compiled to use.
pub const MIN_ENV_SIZE: usize = size_of::<emacs_env>();

/// The type of all Emacs subroutines.
pub type EmacsSubr = unsafe extern "C" fn(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut os::raw::c_void,
) -> emacs_value;
