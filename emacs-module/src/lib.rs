// These crate-wide attrs are useful because `bindgen`:

// generates types that don't and can't conform to the Rust naming conventions.
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

// generates a lot of constants that aren't used in this crate directly
#![allow(dead_code)]

extern crate libc;

/// The type of all Emacs subroutines.
pub type EmacsSubr =
    unsafe extern "C" fn(env: *mut emacs_env,
                         nargs: libc::ptrdiff_t,
                         args: *mut emacs_value,
                         data: *mut libc::c_void) -> emacs_value;

pub type EmacsVal = emacs_value;

include!(concat!(env!("OUT_DIR"), "/emacs_module.rs"));
