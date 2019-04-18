macro_rules! raw_fn {
    ($env:ident, $name:ident) => { {
        (*$env.raw).$name.expect(stringify!(Required module function does not exist: $name))
    }};
}

macro_rules! raw_call_no_exit {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        unsafe {
            // println!("raw_call_no_exit {:?}", stringify!($name));
            let $name = raw_fn!($env, $name);
            $name($env.raw $(, $args)*)
        }
    };
}

macro_rules! raw_call {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        {
            // println!("raw_call {:?}", stringify!($name));
            let env = $env;
            let result = unsafe {
                let $name = raw_fn!(env, $name);
                $name(env.raw $(, $args)*)
            };
            env.handle_exit(result)
        }
    };
}

macro_rules! raw_call_value {
    ($env:ident, $name:ident $(, $args:expr)*) => {
        {
            // println!("raw_call_value {:?}", stringify!($name));
            let result: $crate::Result<$crate::raw::emacs_value> = raw_call!($env, $name $(, $args)*);
            result.map(|raw| unsafe {
                // TODO: In some cases, we can get away without protection. Try optimizing them.
                $crate::Value::new_protected(raw, $env)
            })
        }
    };
}

// TODO: Export this.
macro_rules! call_lisp {
    ($env:ident, $name:expr $(, $arg:expr)*) => {
        {
            // println!("call_lisp {:?}", $name);
            let symbol: $crate::Value<'_> = $env.intern($name)?;
            let args = &mut [$($arg.raw,)*];
            raw_call_value!($env, funcall, symbol.raw, args.len() as $crate::deps::libc::ptrdiff_t, args.as_mut_ptr())
        }
    };
}

macro_rules! enable_transfers {
    ($($name:ident;)*) => {$(
        impl<T> $crate::Transfer for $name<T> {
            fn type_name() -> &'static str { stringify!($name) }
        }

        impl<'e, T> $crate::IntoLisp<'e> for $name<T> {
            fn into_lisp(self, env: &$crate::Env) -> $crate::Result<$crate::Value<'_>> {
                ::std::boxed::Box::new(self).into_lisp(env)
            }
        }
    )*};
}

/// Declares that this module is GPL-compatible. Emacs will not load it otherwise.
#[macro_export]
#[allow(non_snake_case)]
macro_rules! plugin_is_GPL_compatible {
    () => {
        /// This states that the module is GPL-compliant.
        /// Emacs won't load the module if this symbol is undefined.
        #[no_mangle]
        #[allow(non_upper_case_globals)]
        pub static plugin_is_GPL_compatible: $crate::deps::libc::c_int = 0;
    };
}

// TODO: Deprecate this in favor of #[module].

/// Registers a function as the initialization hook. [`#[module]`] is preferred over this low-level
/// interface.
///
/// This declares `emacs_module_init` and `emacs_rs_module_init`, by wrapping the given function,
/// whose signature must be `fn(&Env) -> Result<Value>`.
///
/// [`#[module]`]: ../emacs_macros/attr.module.html
#[macro_export]
macro_rules! module_init {
    ($init:ident) => {
        /// Entry point for Emacs's module loader.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_module_init(
            runtime: *mut $crate::raw::emacs_runtime,
        ) -> $crate::deps::libc::c_int {
            $crate::func::HandleInit::handle_init($crate::Env::from_runtime(runtime), $init)
        }

        // TODO: Exclude this in release build.
        /// Entry point for live-reloading (by `rs-module`) during development.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_rs_module_init(
            raw: *mut $crate::raw::emacs_env,
        ) -> $crate::deps::libc::c_int {
            $crate::func::HandleInit::handle_init($crate::Env::new(raw), $init)
        }
    };
}

// TODO: Consider making this a function, using `data` to do the actual routing, like in
// https://github.com/Wilfred/remacs/pull/516.
#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! lambda {
    // Default function-specific data is (unused) null pointer.
    ($env:expr, $func:path, $arities:expr, $doc:expr $(,)*) => {
        lambda!($env, $func, $arities, $doc, ::std::ptr::null_mut())
    };

    // Default doc string is empty.
    ($env:expr, $func:path, $arities:expr $(,)*) => {
        lambda!($env, $func, $arities, "")
    };

    // Declare a wrapper function.
    ($env:expr, $func:path, $arities:expr, $doc:expr, $data:expr $(,)*) => {{
        use $crate::func::HandleCall;
        use $crate::func::Manage;
        // TODO: Generate identifier from $func.
        unsafe extern "C" fn extern_lambda(
            env: *mut $crate::raw::emacs_env,
            nargs: $crate::deps::libc::ptrdiff_t,
            args: *mut $crate::raw::emacs_value,
            data: *mut $crate::deps::libc::c_void,
        ) -> $crate::raw::emacs_value {
            let env = $crate::Env::new(env);
            let env = $crate::CallEnv::new(env, nargs, args, data);
            env.handle_call($func)
        }

        // Safety: The raw pointer $data is simply ignored.
        unsafe { $env.make_function(extern_lambda, $arities, $doc, $data) }
    }};
}

// TODO: Use `$crate::` instead of `local_inner_macros` once everyone is on 1.30.
// See https://doc.rust-lang.org/nightly/edition-guide/rust-2018/macros/macro-changes.html#macros-using-local_inner_macros.
/// Exports Rust functions to the Lisp runtime. [`#[defun]`] is preferred over this low-level
/// interface.
///
/// [`#[defun]`]: ../emacs_macros/attr.defun.html
#[macro_export(local_inner_macros)]
macro_rules! export_functions {
    // Cut trailing comma in top-level.
    ($env:expr, $prefix:expr, $mappings:tt,) => {
        export_functions!($env, $prefix, $mappings)
    };
    // Cut trailing comma in mappings.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),+,
    }) => {
        export_functions!($env, $prefix, {
            $( $name => $declaration ),*
        })
    };
    // Expand each mapping.
    ($env:expr, $prefix:expr, {
        $( $name:expr => $declaration:tt ),*
    }) => {
        {
            use $crate::func::Manage;
            $( export_functions!(decl, $env, $prefix, $name, $declaration)?; )*
        }
    };

    // Cut trailing comma in declaration.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+,)) => {
        export_functions!(decl, $env, $prefix, $name, ($func, $( $opt ),*))
    };
    // Create a function and set a symbol to it.
    (decl, $env:expr, $prefix:expr, $name:expr, ($func:path, $( $opt:expr ),+)) => {
        $env.fset(
            &_emacs_format!("{}{}", $prefix, $name),
            lambda!($env, $func, $($opt),*)?
        )
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! _emacs_format {
    ($($inner:tt)*) => {
        format!($($inner)*)
    }
}

#[deprecated(since = "0.7.0", note = "Please use `emacs::plugin_is_GPL_compatible!` instead")]
#[doc(hidden)]
#[macro_export(local_inner_macros)]
#[allow(non_snake_case)]
macro_rules! emacs_plugin_is_GPL_compatible {
    ($($inner:tt)*) => {
        plugin_is_GPL_compatible!($($inner)*);
    };
}

#[deprecated(since = "0.7.0", note = "Please use `#[emacs::module]` instead")]
#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! emacs_module_init {
    ($($inner:tt)*) => {
        module_init!($($inner)*);
    };
}

#[deprecated(since = "0.7.0", note = "Please use `#[defun]` instead")]
#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! emacs_export_functions {
    ($($inner:tt)*) => {
        export_functions!($($inner)*)
    };
}

#[deprecated(since = "0.7.0", note = "Please use `emacs::lambda!` instead")]
#[doc(hidden)]
#[macro_export(local_inner_macros)]
macro_rules! emacs_lambda {
    ($($inner:tt)*) => {
        lambda!($($inner)*)
    };
}
