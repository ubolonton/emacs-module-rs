#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use libc;
use std::os::raw;

/// The type of all Emacs subroutines.
pub type EmacsSubr =
    unsafe extern "C" fn(env: *mut EmacsEnv,
                         nargs: libc::ptrdiff_t,
                         args: *mut EmacsVal,
                         data: *mut raw::c_void) -> EmacsVal;

/// Emacs environment.
pub type EmacsEnv = emacs_env;

/// Emacs runtime
pub type EmacsRT = emacs_runtime;

/// Emacs value.
pub type EmacsVal = emacs_value;


include!(concat!(env!("OUT_DIR"), "/emacs_module.rs"));
