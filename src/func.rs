use std::os::raw;
use new::Env;
use emacs_gen::{EmacsSubr, EmacsVal};
use error::Result;

pub trait Func {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal>;
    fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal>;
}

impl Func for Env {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal> {
        raw_call!(self, make_function, min_arity, max_arity, Some(function), doc, data)
    }

    fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal> {
        self.call("fset", &mut [
            self.intern(name)?, func
        ])
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
