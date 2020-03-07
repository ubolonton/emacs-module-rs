use std::borrow::BorrowMut;

use emacs_module::emacs_value;
use emacs_macros;

use crate::{Env, Value, Result, IntoLisp, GlobalRef, types::OnceGlobalRef};

// TODO: Seal this trait, for safety reasons.
pub unsafe trait IntoLispArgs<'e> {
    type LispArgs: BorrowMut<[emacs_value]>;

    fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs>;
}

impl<'e> Value<'e> {
    /// Calls this value with the given arguments. An error is signaled if it is actually not a
    /// Lisp's callable.
    ///
    /// `args` should be an array/slice of `Value`, or a tuple of different types, each implementing
    /// [`IntoLisp`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use emacs::{defun, Value, Result, Vector};
    /// #[defun]
    /// fn mapc_enumerate_vec(function: Value, vector: Vector) -> Result<()> {
    ///     for nth in 0..vector.size()? {
    ///         let elem: Value = vector.get(nth)?;
    ///         function.call((nth, elem))?;
    ///     }
    ///     Ok(())
    /// }
    /// ```
    ///
    /// [`IntoLisp`]: trait.IntoLisp.html
    pub fn call<A>(self, args: A) -> Result<Value<'e>> where A: IntoLispArgs<'e> {
        let env = self.env;
        let mut lisp_args = args.into_lisp_args(env)?;
        let lisp_args: &mut [emacs_value] = lisp_args.borrow_mut();
        let ptr = lisp_args.as_mut_ptr();
        let length = lisp_args.len() as isize;
        // Safety:
        // - ptr comes from a locally-owned value.
        // - length is ensured to be valid by IntoLispArgs implementation.
        unsafe_raw_call_value!(env, funcall, self.raw, length, ptr)
    }
}

pub trait IntoLispCallable<'e> {
    fn into_lisp_callable(self, env: &'e Env) -> Result<Value<'e>>;
}

impl Env {
    /// Calls a Lisp function, passing the given arguments.
    ///
    /// - `func` should be a string, or a Lisp's callable [`Value`] (in which case [`func.call`]
    /// is preferable). An error is signaled otherwise.
    /// - `args` should be an array/slice of [`Value`], or a tuple of different types, each
    /// implementing [`IntoLisp`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use emacs::{defun, Value, Result, Vector};
    /// #[defun]
    /// fn listify_vec(vector: Vector) -> Result<Value> {
    ///     let env = vector.0.env;
    ///     let mut args = vec![];
    ///     for i in 0..vector.size()? {
    ///         args.push(vector.get(i)?)
    ///     }
    ///     env.call("list", &args)
    /// }
    /// ```
    ///
    /// [`Value`]: struct.Value.html
    /// [`func.call`]: struct.Value.html#method.call
    /// [`IntoLisp`]: trait.IntoLisp.html
    #[inline]
    pub fn call<'e, F, A>(&'e self, func: F, args: A) -> Result<Value<'_>>
    where
        F: IntoLispCallable<'e>,
        A: IntoLispArgs<'e>,
    {
        func.into_lisp_callable(self)?.call(args)
    }
}

impl GlobalRef {
    /// Calls this reference's value with the given arguments. An error is signaled if it is
    /// actually not a Lisp's callable.
    ///
    /// `args` should be an array/slice of `Value`, or a tuple of different types, each implementing
    /// [`IntoLisp`].
    ///
    /// [`IntoLisp`]: trait.IntoLisp.html
    #[inline]
    pub fn call<'e, A>(&'e self, env: &'e Env, args: A) -> Result<Value<'_>>
        where
            A: IntoLispArgs<'e>,
    {
        self.bind(env).call(args)
    }
}

// We can implement IntoLispArgs for IntoLisp types (after breaking up this implementation).
// However, that would prevent implementing IntoLisp for arrays and tuples. It is a choice between
// the convenience of not having to use (,) when calling functions with 1 argument, and the
// convenience of not having to use .into_lisp(env)? when returning an array/tuple from a #[defun].
// TODO: Check again when specialization lands: https://github.com/rust-lang/rust/issues/31844.
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

impl<'e> IntoLispCallable<'e> for &'e GlobalRef {
    #[inline(always)]
    fn into_lisp_callable(self, env: &'e Env) -> Result<Value<'e>> {
        self.bind(env).into_lisp_callable(env)
    }
}

impl<'e> IntoLispCallable<'e> for &'e OnceGlobalRef {
    #[inline(always)]
    fn into_lisp_callable(self, env: &'e Env) -> Result<Value<'e>> {
        self.bind(env).into_lisp_callable(env)
    }
}
