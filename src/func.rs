//! Machinery for defining and exporting functions to the Lisp runtime. It should be mainly used by
//! the #[[`defun`]] macro, not module code.
//!
//! [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html

use std::{
    os, panic,
    ffi::CString,
    ops::{Deref, Range},
    slice,
};

use emacs_module::{emacs_value, EmacsSubr};

use crate::{Env, Value, Result, FromLisp, IntoLisp};

/// Exports Rust functions to the Lisp runtime. #[[`defun`]] is preferred over this low-level
/// interface.
///
/// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
#[deprecated(since = "0.11.0", note = "Please use `#[defun]` instead")]
#[macro_export]
macro_rules! export_functions {
    ($($inner:tt)*) => {
        $crate::__export_functions!($($inner)*)
    };
}

#[deprecated(since = "0.7.0", note = "Please use `#[defun]` instead")]
#[doc(hidden)]
#[macro_export]
macro_rules! emacs_export_functions {
    ($($inner:tt)*) => {
        $crate::__export_functions!($($inner)*)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __export_functions {
    // Cut trailing comma in top-level.
    ($env:expr, $prefix:expr, $mappings:tt,) => {
        $crate::__export_functions!($env, $prefix, $mappings)
    };
    // Cut trailing comma in mappings.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),+,
    }) => {
        $crate::__export_functions!($env, $prefix, {
            $( $name => $declaration ),*
        })
    };
    // Expand each mapping.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),*
    }) => {
        {
            use $crate::func::Manage;
            $( $crate::__export_functions!(decl, $env, $prefix, $name, $declaration)?; )*
        }
    };

    // Cut trailing comma in declaration.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+,)) => {
        $crate::__export_functions!(decl, $env, $prefix, $name, ($func, $( $opt ),*))
    };
    // Create a function and set a symbol to it.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+)) => {
        $env.fset(
            &format!("{}{}", $prefix, $name),
            $crate::lambda!($env, $func, $($opt),*)?
        )
    };
}

// TODO: Support closures, storing them in the data pointer, using a single handler to dispatch.
#[doc(hidden)]
#[macro_export]
macro_rules! lambda {
    // Default doc string is empty.
    ($env:expr, $func:path, $arities:expr $(,)*) => {
        $crate::lambda!($env, $func, $arities, "")
    };

    // Declare a wrapper function.
    ($env:expr, $func:path, $arities:expr, $doc:expr $(,)*) => {
        {
            use $crate::func::HandleCall;
            use $crate::func::Manage;
            // TODO: Generate identifier from $func.
            unsafe extern "C" fn extern_lambda(
                env: *mut $crate::raw::emacs_env,
                nargs: isize,
                args: *mut $crate::raw::emacs_value,
                _data: *mut ::std::os::raw::c_void,
            ) -> $crate::raw::emacs_value {
                let env = $crate::Env::new(env);
                let env = $crate::CallEnv::new(env, nargs, args);
                env.handle_call($func)
            }

            // Safety: The raw pointer is simply ignored.
            unsafe { $env.make_function(extern_lambda, $arities, $doc, ::std::ptr::null_mut()) }
        }
    };
}

#[deprecated(since = "0.7.0", note = "Please use `emacs::lambda!` instead")]
#[doc(hidden)]
#[macro_export]
macro_rules! emacs_lambda {
    ($($inner:tt)*) => {
        $crate::lambda!($($inner)*)
    };
}

pub trait Manage {
    unsafe fn make_function<T: Into<Vec<u8>>>(
        &self,
        function: EmacsSubr,
        arities: Range<usize>,
        doc: T,
        data: *mut os::raw::c_void,
    ) -> Result<Value<'_>>;

    fn fset(&self, name: &str, func: Value<'_>) -> Result<Value<'_>>;
}

impl Manage for Env {
    /// # Safety
    ///
    /// The `function` must use `data` pointer in safe ways.
    #[allow(unused_unsafe)]
    unsafe fn make_function<T: Into<Vec<u8>>>(
        &self,
        function: EmacsSubr,
        arities: Range<usize>,
        doc: T,
        data: *mut os::raw::c_void,
    ) -> Result<Value<'_>> {
        raw_call_value!(
            self,
            make_function,
            arities.start as isize,
            arities.end as isize,
            Some(function),
            CString::new(doc)?.as_ptr(),
            data
        )
    }

    fn fset(&self, name: &str, func: Value<'_>) -> Result<Value<'_>> {
        let symbol = self.intern(name)?;
        self.call("fset", [symbol, func])
    }
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
}

// TODO: Iterator and Index
impl CallEnv {
    #[doc(hidden)]
    #[inline]
    pub unsafe fn new(
        env: Env,
        nargs: isize,
        args: *mut emacs_value,
    ) -> Self {
        let nargs = nargs as usize;
        Self { env, nargs, args }
    }

    #[doc(hidden)]
    #[inline]
    pub fn raw_args(&self) -> &[emacs_value] {
        // Safety: Emacs assures *args is valid for the duration of the call, with length nargs.
        unsafe { slice::from_raw_parts(self.args, self.nargs) }
    }

    pub fn args(&self) -> Vec<Value<'_>> {
        // Safety: Emacs assures *args are on the stack for the duration of the call.
        self.raw_args().iter().map(|v| unsafe { Value::new(*v, &self.env) }).collect()
    }

    #[inline]
    pub fn get_arg(&self, i: usize) -> Value<'_> {
        let args: &[emacs_value] = self.raw_args();
        // Safety: Emacs assures *args are on the stack for the duration of the call.
        unsafe { Value::new(args[i], &self) }
    }

    #[inline]
    pub fn parse_arg<'e, T: FromLisp<'e>>(&'e self, i: usize) -> Result<T> {
        self.get_arg(i).into_rust()
    }
}

/// This allows `Env`'s methods to be called on a `CallEnv`.
impl Deref for CallEnv {
    type Target = Env;

    #[doc(hidden)]
    #[inline(always)]
    fn deref(&self) -> &Env {
        &self.env
    }
}

pub trait HandleCall {
    fn handle_call<'e, T, F>(&'e self, f: F) -> emacs_value
        where
            F: Fn(&'e CallEnv) -> Result<T> + panic::RefUnwindSafe,
            T: IntoLisp<'e>;
}

impl HandleCall for CallEnv {
    #[inline]
    fn handle_call<'e, T, F>(&'e self, f: F) -> emacs_value
    where
        F: Fn(&'e CallEnv) -> Result<T> + panic::RefUnwindSafe,
        T: IntoLisp<'e>,
    {
        let env = panic::AssertUnwindSafe(self);
        let result = panic::catch_unwind(|| unsafe {
            let rust_result = f(&env);
            let lisp_result = rust_result.and_then(|t| t.into_lisp(&env));
            env.maybe_exit(lisp_result)
        });
        env.handle_panic(result)
    }
}
