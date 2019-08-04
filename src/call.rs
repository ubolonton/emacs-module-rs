use std::borrow::BorrowMut;

use emacs_module::emacs_value;
use emacs_macros;

use crate::{Env, Value, Result, IntoLisp};

// TODO: Seal this trait, for safety reasons.
pub unsafe trait IntoLispArgs<'e> {
    type LispArgs: BorrowMut<[emacs_value]>;

    fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs>;
}

pub trait IntoLispCallable<'e> {
    fn into_lisp_callable(self, env: &'e Env) -> Result<Value<'e>>;
}

impl Env {
    pub fn call<'e, F, A>(&'e self, function: F, args: A) -> Result<Value<'_>>
    where
        F: IntoLispCallable<'e>,
        A: IntoLispArgs<'e>,
    {
        let callable = function.into_lisp_callable(self)?;
        let mut lisp_args = args.into_lisp_args(self)?;
        let lisp_args: &mut [emacs_value] = lisp_args.borrow_mut();
        let ptr = lisp_args.as_mut_ptr();
        let length = lisp_args.len() as isize;
        raw_call_value!(self, funcall, callable.raw, length, ptr)
    }
}

unsafe impl IntoLispArgs<'_> for &[Value<'_>] {
    type LispArgs = Vec<emacs_value>;

    fn into_lisp_args(self, _: &Env) -> Result<Self::LispArgs> {
        Ok(self.iter().map(|v| v.raw).collect())
    }
}

// TODO: With this, () would become a single argument with value nil, which is correct, but looks
// surprising.
unsafe impl<'e, T: IntoLisp<'e>> IntoLispArgs<'e> for T {
    type LispArgs = [emacs_value; 1];

    #[inline]
    fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs> {
        Ok([self.into_lisp(env)?.raw])
    }
}

emacs_macros::impl_lisp_args_for_tuples_with_max_arity!(12);

emacs_macros::impl_lisp_args_for_arrays_of_max_length!(12);

impl<'e> IntoLispCallable<'e> for Value<'e> {
    #[inline(always)]
    fn into_lisp_callable(self, _: &'e Env) -> Result<Value<'e>> {
        Ok(self)
    }
}

impl<'e, T: AsRef<str>> IntoLispCallable<'e> for T {
    #[inline(always)]
    fn into_lisp_callable(self, env: &'e Env) -> Result<Value<'e>> {
        env.intern(self.as_ref())
    }
}
