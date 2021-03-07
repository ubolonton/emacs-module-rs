/// Gets the raw function with the given name.
macro_rules! raw_fn {
    ($env:expr, $name:ident) => { {
        (*$env.raw).$name.expect(stringify!(Required module function does not exist: $name))
    }};
}

/// Calls a raw function, without checking for non-local exit afterwards. Use this if the C
/// implementation never calls `module_non_local_exit_signal_1`.
/// TODO: Check for situations that `MODULE_HANDLE_NONLOCAL_EXIT` calls `module_out_of_memory`.
macro_rules! unsafe_raw_call_no_exit {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        unsafe {
            let $name = raw_fn!($env, $name);
            $name($env.raw $(, $args)*)
        }
    };
}

/// Calls a raw function, then handles any pending non-local exit.
macro_rules! unsafe_raw_call {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        {
            let env = $env;
            let result = unsafe {
                let $name = raw_fn!(env, $name);
                $name(env.raw $(, $args)*)
            };
            env.handle_exit(result)
        }
    };
}

/// Calls a raw function that returns an emacs_value, then handles any pending non-local exit.
/// Returns a [`Value`].
///
/// [`Value`]: struct.Value.html
macro_rules! unsafe_raw_call_value {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        unsafe_raw_call_value_unprotected!($env, $name $(, $args)*).map(|v| v.protect())
    };
}

/// Like [`unsafe_raw_call_value!`], except that the returned [`Value`] is not protected against
/// Emacs GC's [bug #31238], which caused [issue #2].
///
/// # Safety
///
/// This can be used as an optimization, in situations when the returned [`Value`] is unused,
/// or when its usage is shorter than the lifespan of the underlying Lisp object.
///
/// [`unsafe_raw_call_value!`]: macro.unsafe_raw_call_value.html
/// [`Value`]: struct.Value.html
/// [bug #31238]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238
/// [issue #2]: https://github.com/ubolonton/emacs-module-rs/issues/2
macro_rules! unsafe_raw_call_value_unprotected {
    ($env:expr, $name:ident $(, $args:expr)*) => {
        {
            let result: $crate::Result<$crate::raw::emacs_value> = unsafe_raw_call!($env, $name $(, $args)*);
            result.map(|raw| unsafe {
                $crate::Value::new(raw, $env)
            })
        }
    };
}

/// Declare global references. These should be initialized when the module is loaded.
macro_rules! global_refs {
    ($($name:ident)*) => {
        $(pub static $name: &'static $crate::global::OnceGlobalRef = {
            static x: $crate::global::OnceGlobalRef = $crate::global::OnceGlobalRef::new();
            &x
        };)*
    };
    ($registrator_name:ident ($init_method:ident) =>
        $(
            $name:ident $( => $lisp_name:expr )?
        )*
    ) => {
        global_refs! {
            $($name)*
        }

        #[$crate::deps::ctor::ctor]
        fn $registrator_name() {
            $crate::init::__PRE_INIT__.try_lock()
                .expect("Failed to acquire a write lock on the list of initializers")
                .push(Box::new(|env| {
                    $(
                        #[allow(unused_variables)]
                        let name = ::emacs_macros::lisp_name!($name);
                        $( let name = $lisp_name; )?
                        $crate::global::OnceGlobalRef::$init_method(&$name, env, name)?;
                    )*
                    Ok(())
                }));
        }
    };
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
        pub static plugin_is_GPL_compatible: ::std::os::raw::c_int = 0;
    };
}

#[deprecated(since = "0.7.0", note = "Please use `emacs::plugin_is_GPL_compatible!` instead")]
#[doc(hidden)]
#[macro_export]
#[allow(non_snake_case)]
macro_rules! emacs_plugin_is_GPL_compatible {
    ($($inner:tt)*) => {
        $crate::plugin_is_GPL_compatible!($($inner)*);
    };
}
