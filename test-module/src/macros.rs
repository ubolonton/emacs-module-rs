//! Experimental utility macros that make writing a module easier. If they prove to be useful, we
//! will move them into the lib after stabilizing them.

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

macro_rules! custom_types {
    ($($name:ident;)*) => {$(
        impl ::emacs::Transfer for $name {}
    )*};
}

macro_rules! call {
    ($env:ident, $name:expr $(, $arg:expr)*) => {{
        use emacs::IntoLisp;
        let args = &[$($arg.into_lisp($env)?,)*];
        $env.call($name, args)
    }}
}
