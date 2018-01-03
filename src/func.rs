use std::ffi::CString;
use std::ops::Range;
use libc;
use emacs_module::{EmacsSubr, EmacsVal};
use super::Env;
use super::error::Result;

// TODO: Consider checking for existence of these upon startup, not on each call.
macro_rules! raw_fn {
    ($env:ident, $name:ident) => {
        (*$env.raw).$name.ok_or($crate::error::Error {
            kind: $crate::error::ErrorKind::CoreFnMissing(format!("{}", stringify!($name)))
        })
    };
}

macro_rules! raw_call {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        {
            let result = unsafe {
                let $name = raw_fn!($env, $name)?;
                $name($env.raw $(, $args)*)
            };
            $crate::error::HandleExit::handle_exit($env, result)
        }
    };
}

#[macro_export]
macro_rules! emacs_plugin_is_GPL_compatible {
    () => {
        /// This states that the module is GPL-compliant.
        /// Emacs won't load the module if this symbol is undefined.
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        pub static plugin_is_GPL_compatible: libc::c_int = 0;
    }
}

/// Declares `emacs_module_init` and `emacs_rs_module_init`, by wrapping the given function, whose
/// signature must be `fn(&mut Env) -> Result<EmacsVal>`.
#[macro_export]
macro_rules! emacs_module_init {
    ($init:ident) => {
        /// Entry point for Emacs's module loader.
        #[no_mangle]
        pub extern "C" fn emacs_module_init(raw: *mut $crate::raw::emacs_runtime) -> ::libc::c_int {
            match $init(&mut $crate::Env::from(raw)) {
                Ok(_) => 0,
                // TODO: Try to signal error to Emacs as well
                Err(_) => 1,
            }
        }

        // TODO: Exclude this in release build.
        /// Entry point for live-reloading (by `rs-module`) during development.
        #[no_mangle]
        pub extern "C" fn emacs_rs_module_init(raw: *mut $crate::raw::emacs_env) -> ::libc::c_int {
            match $init(&mut $crate::Env::from(raw)) {
                Ok(_) => 0,
                // TODO: Try to signal error to Emacs as well
                Err(_) => 1,
            }
        }
    };
}

// TODO: This should be named sth like HandleSubr, HandleRawFn
// TODO: Enable creating a Lisp function from a Rust fn. That probably requires procedural macros,
// macro_rules! is inadequate.
pub trait HandleFunc {
    fn make_function(&mut self, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<EmacsVal>;
    fn fset(&mut self, name: &str, func: EmacsVal) -> Result<EmacsVal>;
    fn register(&mut self, name: &str, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<EmacsVal>;
}

impl HandleFunc for Env {
    fn make_function(&mut self, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<EmacsVal> {
        raw_call!(
            self, make_function,
            arities.start as isize, arities.end as isize,
            Some(function), CString::new(doc)?.as_ptr(), data
        )
    }

    fn fset(&mut self, name: &str, func: EmacsVal) -> Result<EmacsVal> {
        let symbol = self.intern(name)?;
        self.call("fset", &mut [symbol, func])
    }

    fn register(&mut self, name: &str, function: EmacsSubr, arities: Range<usize>, doc: &str, data: *mut libc::c_void) -> Result<EmacsVal> {
        let function = self.make_function(function, arities, doc, data)?;
        self.fset(name, function)
    }
}

#[macro_export]
macro_rules! emacs_subrs {
    ($($name:ident -> $extern_name:ident;)*) => {
        $(
            #[allow(non_snake_case, unused_variables)]
            unsafe extern "C" fn $extern_name(env: *mut $crate::raw::emacs_env,
                                              nargs: libc::ptrdiff_t,
                                              args: *mut $crate::EmacsVal,
                                              data: *mut libc::c_void) -> $crate::EmacsVal {
                let mut env = $crate::Env::from(env);
                let args: &[$crate::EmacsVal] = std::slice::from_raw_parts(args, nargs as usize);
                let result = $name(&mut env, args, data);
                $crate::error::TriggerExit::maybe_exit(&mut env, result)
            }
        )*
    };
}
