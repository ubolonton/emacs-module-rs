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
    /// Calls a Lisp function, passing the given arguments.
    ///
    /// - `func` should be a string, or a Lisp's callable [`Value`]. An error is signaled otherwise.
    /// - `args` should be an array/slice of [`Value`], or a tuple of different types,
    /// each implementing [`IntoLisp`].
    ///
    /// [`IntoLisp`]: trait.IntoLisp.html
    /// [`Value`]: struct.Value.html
    pub fn call<'e, F, A>(&'e self, func: F, args: A) -> Result<Value<'_>>
    where
        F: IntoLispCallable<'e>,
        A: IntoLispArgs<'e>,
    {
        let callable = func.into_lisp_callable(self)?;
        let mut lisp_args = args.into_lisp_args(self)?;
        let lisp_args: &mut [emacs_value] = lisp_args.borrow_mut();
        let ptr = lisp_args.as_mut_ptr();
        let length = lisp_args.len() as isize;
        raw_call_value!(self, funcall, callable.raw, length, ptr)
    }

    pub fn list<'e, A: IntoLispArgs<'e>>(&'e self, args: A) -> Result<Value<'_>> {
        self.call("list", args)
    }
}

unsafe impl<'e, T: AsRef<[Value<'e>]> + ?Sized> IntoLispArgs<'e> for &T {
    type LispArgs = Vec<emacs_value>;

    fn into_lisp_args(self, _: &'e Env) -> Result<Self::LispArgs> {
        Ok(self.as_ref().iter().map(|v| v.raw).collect())
    }
}

emacs_macros::impl_lisp_args_for_tuples!(12);

emacs_macros::impl_lisp_args_for_arrays!(12);

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
