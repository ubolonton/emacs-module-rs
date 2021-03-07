#![allow(non_upper_case_globals)]

use crate::{Env, Result, Value, global::{GlobalRef, OnceGlobalRef}};

global_refs! {common(init_to_symbol) =>
    nil t
    error
    rust_error
    rust_panic
    rust_wrong_type_user_ptr
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
