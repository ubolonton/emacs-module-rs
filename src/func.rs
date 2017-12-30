use std::os::raw;
use Env;
use emacs_gen::{EmacsSubr, EmacsVal};
use error::Result;

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

pub trait HandleFunc {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal>;
    fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal>;
    fn register(&self, name: &str, function: EmacsSubr, min_arity: isize, max_arity: isize, doc: &str, data: *mut raw::c_void) -> Result<EmacsVal>;
}

impl HandleFunc for Env {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal> {
        raw_call!(self, make_function, min_arity, max_arity, Some(function), doc, data)
    }

    fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal> {
        self.call("fset", &mut [
            self.intern(name)?, func
        ])
    }

    fn register(&self, name: &str, function: EmacsSubr, min_arity: isize, max_arity: isize, doc: &str, data: *mut raw::c_void) -> Result<EmacsVal> {
        self.fset(
            name, self.make_function(
                min_arity, max_arity, function,
                self.to_cstring(doc)?.as_ptr(), data
            )?
        )
    }
}

#[macro_export]
macro_rules! expose_subrs {
    ($($name:ident -> $extern_name:ident;)*) => {
        $(
            #[allow(non_snake_case, unused_variables)]
            unsafe extern "C" fn $extern_name(env: *mut $crate::EmacsEnv,
                                              nargs: libc::ptrdiff_t,
                                              args: *mut $crate::EmacsVal,
                                              data: *mut raw::c_void) -> $crate::EmacsVal {
                let env = &$crate::Env::from(env);
                let args: & [$crate::EmacsVal] = std::slice::from_raw_parts(args, nargs as usize);
                let result = $name(env, args, data);
                $crate::error::TriggerExit::maybe_exit(env, result)
            }
        )*
    };
}
