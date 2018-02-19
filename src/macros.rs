// TODO: Consider checking for existence of these upon startup, not on each call.
macro_rules! raw_fn {
    ($env:ident, $name:ident) => {
        (*$env.raw).$name.ok_or($crate::error::Error {
            kind: $crate::error::ErrorKind::CoreFnMissing(format!("{}", stringify!($name)))
        })
    };
}

macro_rules! raw_call {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        {
            let env = $env;
            let result = unsafe {
                let $name = raw_fn!(env, $name)?;
                $name(env.raw $(, $args)*)
            };
            env.handle_exit(result)
        }
    };
}

macro_rules! raw_call_value {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        {
            let result: $crate::Result<$crate::raw::emacs_value> = raw_call!($env, $name $(, $args)*);
            result.map(|raw| unsafe {
                $crate::Value::new(raw, $env)
            })
        }
    };
}
/// Note: Some functions in emacs-module.h are critically important, like those that support error
/// reporting to Emacs. If they are missing, the only sensible thing to do is crashing. Use this
/// macro to call them instead of [`raw_call!`].
macro_rules! critical {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        unsafe {
            let $name = raw_fn!($env, $name)
                .expect(&format!("Required function {} cannot be found", stringify!($name)));
            $name($env.raw $(, $args)*)
        }
    };
}

// TODO: Export this.
macro_rules! call_lisp {
    ($env:ident, $name:expr $(, $arg:expr)*) => {
        {
            let symbol: $crate::Value = $env.intern($name)?;
            let args = &mut [$($arg.raw,)*];
            raw_call_value!($env, funcall, symbol.raw, args.len() as ::libc::ptrdiff_t, args.as_mut_ptr())
        }
    };
}

macro_rules! enable_transfers {
    ($($name:ident;)*) => {$(
        impl<T> $crate::Transfer for $name<T> {
            fn type_name() -> &'static str { stringify!($name) }
        }

        impl<'e, T> $crate::IntoLisp<'e> for $name<T> {
            fn into_lisp(self, env: &$crate::Env) -> $crate::Result<$crate::Value> {
                ::std::boxed::Box::new(self).into_lisp(env)
            }
        }
    )*};
}

/// Declares that this module is GPL-compatible. Emacs will not load it otherwise.
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
/// signature must be `fn(&Env) -> Result<Value>`.
#[macro_export]
macro_rules! emacs_module_init {
    ($init:ident) => {
        /// Entry point for Emacs's module loader.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_module_init(runtime: *mut $crate::raw::emacs_runtime) -> ::libc::c_int {
            $crate::func::HandleInit::handle_init($crate::Env::from_runtime(runtime), $init)
        }

        // TODO: Exclude this in release build.
        /// Entry point for live-reloading (by `rs-module`) during development.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_rs_module_init(raw: *mut $crate::raw::emacs_env) -> ::libc::c_int {
            $crate::func::HandleInit::handle_init($crate::Env::new(raw), $init)
        }
    };
}

// TODO: Consider making this a function, using `data` to do the actual routing, like in
// https://github.com/Wilfred/remacs/pull/516.
#[macro_export]
macro_rules! emacs_lambda {
    // Default function-specific data is (unused) null pointer.
    ($env:expr, $func:path, $arities:expr, $doc:expr $(,)*) => {
        emacs_lambda!($env, $func, $arities, $doc, ::std::ptr::null_mut())
    };

    // Default doc string is empty.
    ($env:expr, $func:path, $arities:expr $(,)*) => {
        emacs_lambda!($env, $func, $arities, "")
    };

    // Declare a wrapper function.
    ($env:expr, $func:path, $arities:expr, $doc:expr, $data:expr $(,)*) => {
        {
            use $crate::func::HandleCall;
            use $crate::func::Manage;
            // TODO: Generate identifier from $func.
            unsafe extern "C" fn extern_lambda(env: *mut $crate::raw::emacs_env,
                                               nargs: ::libc::ptrdiff_t,
                                               args: *mut $crate::raw::emacs_value,
                                               data: *mut ::libc::c_void) -> $crate::raw::emacs_value {
                let env = $crate::Env::new(env);
                let env = $crate::CallEnv::new(env, nargs, args, data);
                env.handle_call($func)
            }

            $env.make_function(extern_lambda, $arities, $doc, $data)
        }
    };
}

/// Export Rust functions so that Lisp code can call them by name.
#[macro_export]
macro_rules! emacs_export_functions {
    // Cut trailing comma in top-level.
    ($env:expr, $prefix:expr, $mappings:tt,) => {
        emacs_export_functions!($env, $prefix, $mappings)
    };
    // Cut trailing comma in mappings.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),+,
    }) => {
        emacs_export_functions!($env, $prefix, {
            $( $name => $declaration ),*
        })
    };
    // Expand each mapping.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),*
    }) => {
        {
            use $crate::func::Manage;
            $( emacs_export_functions!(decl, $env, $prefix, $name, $declaration)?; )*
        }
    };

    // Cut trailing comma in declaration.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+,)) => {
        emacs_export_functions!(decl, $env, $prefix, $name, ($func, $( $opt ),*))
    };
    // Create a function and set a symbol to it.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+)) => {
        $env.fset(
            &format!("{}{}", $prefix, $name),
            emacs_lambda!($env, $func, $($opt),*)?
        )
    };
}
