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
    ($env:expr, $name:expr $(, $arg:expr)*) => {
        {
            let env = $env;
            let symbol: $crate::Value<'_> = env.intern($name)?;
            let mut args = [
                $($crate::IntoLisp::into_lisp($arg, env)?.raw,)*
            ];
            raw_call_value!(env, funcall, symbol.raw, args.len() as isize, args.as_mut_ptr())
        }
    };
}

macro_rules! enable_transfers {
    ($($name:ident;)*) => {$(
        impl<T: 'static> $crate::Transfer for $name<T> {
            fn type_name() -> &'static str { stringify!($name) }
        }

        impl<'e, T: 'static> $crate::IntoLisp<'e> for $name<T> {
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
