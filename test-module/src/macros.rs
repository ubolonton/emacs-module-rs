//! Experimental utility macros that make writing a module easier. If they prove to be useful, we
//! will move them into the lib after stabilizing them.

macro_rules! prefix {
    ($namer:ident, $prefix:ident) => {
        macro_rules! $namer {
            ($name:ident) => {
                $namer!(stringify!($name))
            };
            ($name:expr) => {
                &format!("{}/{}", $prefix, $name)
            };
        }
    };
}
