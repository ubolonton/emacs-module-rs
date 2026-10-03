use crate::{Env, Result, Value, global::{GlobalRef, OnceGlobalRef}};

/// Defines static [`&OnceGlobalRef`] variables that point to corresponding Lisp symbols.
///
/// This macro accepts a space-separated list of identifiers, and determine the Lisp symbol names by
/// replacing underscores with hyphens, or by explicit mappings in the form of `=> "symbol-name"`.
///
/// It can be used only once per Rust `mod`.
///
/// If a symbol is bound to a function, the variable can be used to call the function via
/// [Env::call]. Unlike with [`use_functions!`], this goes through the indirection of symbol lookup.
///
/// [`&OnceGlobalRef`]: OnceGlobalRef
/// [`use_functions!`]: crate::use_functions
#[macro_export]
macro_rules! use_symbols {
    ($( $name:ident $( => $lisp_name:expr )? )*) => {
        $crate::global_refs! {__emrs_init_global_refs_to_symbols__(init_to_symbol) =>
            $( $name $( => $lisp_name )? )*
        }
    }
}

use_symbols! {
    nil t
    error
    rust_error
    rust_panic
    rust_wrong_type_user_ptr
    wrong_type_argument
    args_out_of_range memory_buffer_too_small
    integerp numberp floatp stringp vectorp user_ptr user_ptrp processp pipe_process_p
    unicode_string_p utf_8_string_p
    rust_module_error
    rust_module_wrong_type
    rust_module_non_unicode_string
    rust_module_invalid_utf_8
    rust_module_buffer_too_small
}

pub trait IntoLispSymbol<'e> {
    fn into_lisp_symbol(self, env: &'e Env) -> Result<Value<'e>>;
}

impl<'e> IntoLispSymbol<'e> for Value<'e> {
    #[inline(always)]
    fn into_lisp_symbol(self, _: &'e Env) -> Result<Value<'e>> {
        Ok(self)
    }
}

impl<'e, T: AsRef<str>> IntoLispSymbol<'e> for T {
    #[inline(always)]
    fn into_lisp_symbol(self, env: &'e Env) -> Result<Value<'e>> {
        env.intern(self.as_ref())
    }
}

impl<'e> IntoLispSymbol<'e> for &'e GlobalRef {
    #[inline(always)]
    fn into_lisp_symbol(self, env: &'e Env) -> Result<Value<'e>> {
        self.bind(env).into_lisp_symbol(env)
    }
}

impl<'e> IntoLispSymbol<'e> for &'e OnceGlobalRef {
    #[inline(always)]
    fn into_lisp_symbol(self, env: &'e Env) -> Result<Value<'e>> {
        self.bind(env).into_lisp_symbol(env)
    }
}
