//! Experimental utility macros that make writing a module easier. If they prove to be useful, we
//! will move them into the lib after stabilizing them.

macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}

// https://danielkeep.github.io/tlborm/book/blk-counting.html
macro_rules! count_tts {
    ($($tts:tt)*) => {<[()]>::len(&[$(replace_expr!($tts ())),*])};
}

/// Defines a macro that creates prefixed string.
macro_rules! make_prefix {
    ($prefixer:ident, $prefix:expr) => {
        macro_rules! $prefixer {
            ($name:ident) => {
                $prefixer!(stringify!($name))
            };
            ($name:expr) => {
                &format!("{}{}", $prefix, $name)
            };
        }
    };
}

/// Defines a Lisp function.
///
/// In the following example, the first `env` is an expression (the env handle to define the
/// function). The second `env` is an identifier (name of the env that the defined function gets
/// called with).
///
/// ```
/// defuns! {
///     env, "my-module/";
///     // Defines "my-module/plus"
///     plus, "x + y", (env, x, y) {
///         let result = env.from_emacs(x)? + env.from_emacs(y)?;
///         result.to_lisp(env)
///     }
///     // Defines "my-module/my-identity"
///     "my-identity", "", (_env, x) {
///         Ok(x)
///     }
/// }
/// ```
///
/// TODO: It would be nice to separate the 2 `env`s, like in the following snippet. That's currently
/// not possible though. (See https://github.com/rust-lang/rust/issues/35853: "error: attempted to
/// repeat an expression containing no syntax variables matched as repeating at this depth".)
///
/// ```
/// using!(defuns, env, "my-module/");
/// defuns! {
///     "foo", "foo doc" -> (env, x) { unimplemented() };
///     "bar", "bar doc" -> (env, x, y) { unimplemented() };
/// }
/// ```
///
/// TODO:
/// - Support custom data `*mut libc::c_void`.
/// - Support automatic conversion of arguments .
/// - Support automatic conversion of return value.
/// - Support optional args.
macro_rules! defuns {
    ($env_var:expr, $prefix:expr; $($name:tt, $doc:expr, ($env:ident $(, $arg:ident)*) $body:expr)*) => {
        make_prefix!(emacs_prefix, $prefix);

        $({
            extern crate libc;
            extern crate emacs;
            use emacs::Value;
            use emacs::{Env, CallEnv, Result};
            use emacs::error::TriggerExit;
            use emacs::raw::{emacs_env, emacs_value};

            // TODO: Construct an identifier from $name, to get better debug symbols. Seems hard.
            // See https://github.com/rust-lang/rust/issues/29599 (`concat_idents` is useless),
            // https://github.com/rust-lang/rfcs/pull/1628,
            // and https://crates.io/crates/interpolate_idents (procedural macros, nightly).
            #[allow(non_snake_case, unused_variables)]
            unsafe extern "C" fn extern_name(env: *mut emacs_env,
                                             nargs: libc::ptrdiff_t,
                                             args: *mut emacs_value,
                                             data: *mut libc::c_void) -> emacs_value {
                let env = Env::from(env);
                let env = CallEnv::new(env, nargs, args, data);
                let args: &[emacs_value] = env.raw_args();
                // TODO: Don't do this for zero-arg functions.
                let mut _iter = args.iter();
                // XXX: .unwrap()
                $(let $arg = $crate::Value::new(*_iter.next().unwrap(), &env);)*
                let result = intern_name(&env $(, $arg)*);
                env.maybe_exit(result)
            }

            fn intern_name<'e>($env: &'e CallEnv $(, $arg: Value<'e>)*) -> Result<Value<'e>> {
                $body
            }

            let nargs = count_tts!($($arg)*);
            $env_var.register(emacs_prefix!($name), extern_name, nargs..nargs, $doc, std::ptr::null_mut())?;
        })*
    };
}

macro_rules! custom_types {
    ($($name:ident as $lisp_name:expr;)*) => {$(
        impl emacs::Transfer for $name {
            fn type_name() -> &'static str { $lisp_name }
        }
    )*};
}

macro_rules! call {
    ($env:ident, $name:expr $(, $arg:expr)*) => {{
        let args = &[$($arg.to_lisp($env)?,)*];
        $env.call($name, args)
    }}
}

macro_rules! simplified_subrs {
    ($($name:ident -> $extern_name:ident;)*) => {
        $(
            #[allow(non_snake_case, unused_variables)]
            unsafe extern "C" fn $extern_name(env: *mut ::emacs::raw::emacs_env,
                                              nargs: ::libc::ptrdiff_t,
                                              args: *mut ::emacs::raw::emacs_value,
                                              data: *mut ::libc::c_void) -> ::emacs::raw::emacs_value {
                let env = ::emacs::Env::from(env);
                let env = ::emacs::CallEnv::new(env, nargs, args, data);
                let result = $name(&env);
                ::emacs::error::TriggerExit::maybe_exit(&*env, result)
            }
        )*
    };
}
