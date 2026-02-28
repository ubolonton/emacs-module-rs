/// Defines static [`&OnceGlobalRef`] variables that point to functions identified by the
/// corresponding symbols.
///
/// This macro accepts a space-separated list of identifiers, and determine the Lisp symbol names by
/// replacing underscores with hyphens, or by explicit mappings in the form of `=> "symbol-name"`.
///
/// It can be used only once per Rust `mod`.
///
/// Unlike with [`use_symbols!`], calling the functions through these variables does not involve the
/// indirection of symbol lookup. That means it is faster, and is not affected by symbol rebinding.
///
/// [`&OnceGlobalRef`]: OnceGlobalRef
/// [`use_symbols!`]: crate::use_symbols
#[macro_export]
macro_rules! use_functions {
    ($( $name:ident $( => $lisp_name:expr )? )*) => {
        $crate::global_refs! {__emrs_init_global_refs_to_functions__(init_to_function) =>
            $( $name $( => $lisp_name )? )*
        }
    }
}

use_functions! {
    cons car cdr
    vector make_vector
    list
    message
}
