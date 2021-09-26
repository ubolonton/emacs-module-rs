// These crate-wide attrs are useful because `bindgen`:

// generates types that don't and can't conform to the Rust naming conventions.
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// generates a lot of constants that aren't used in this crate directly
#![allow(dead_code)]

use std::os;

#[cfg(not(feature = "bindgen"))]
include!("./emacs-module.rs");

#[cfg(feature = "bindgen")]
include!(concat!(env!("OUT_DIR"), "/emacs-module.rs"));

/// The type of all Emacs subroutines.
pub type EmacsSubr = unsafe extern "C" fn(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut os::raw::c_void,
) -> emacs_value;
