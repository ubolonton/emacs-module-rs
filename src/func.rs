use std::os::raw;
use new::Env;
use emacs_gen::{EmacsSubr, EmacsVal};
use error::Result;

trait Func {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal>;
    fn fset(&self, name: &str, func: EmacsVal) -> Result<EmacsVal>;
}

impl Func for Env {
    fn make_function(&self, min_arity: isize, max_arity: isize, function: EmacsSubr, doc: *const i8, data: *mut raw::c_void) -> Result<EmacsVal> {
        unsafe {
            let make_function = (*self.raw).make_function.unwrap();
            Ok(make_function(self.raw, min_arity, max_arity, Some(function), doc, data))
        }
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
                let env = $crate::Env::from(env);
                // TODO: Check whether Emacs keeps the ownership of these. If it does, we want to
                // renounce ownership later on without dropping.
                let mut args = Vec::<$crate::EmacsVal>::from_raw_parts(args, nargs as usize, nargs as usize);
                let result = $name(&env, &mut args, data);
                $crate::error::TriggerExit::maybe_exit(&env, result)
            }
        )*
    };
}
