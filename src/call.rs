use std::borrow::BorrowMut;

use emacs_module::emacs_value;
use emacs_macros;

use crate::{Env, Value, Result, IntoLisp};

// TODO: Seal this trait, for safety reasons.
pub unsafe trait IntoLispArgs<'e> {
    type LispArgs: BorrowMut<[emacs_value]>;

    fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs>;
}

impl Env {
    // TODO: Add a convenient macro?
    pub fn call_old(&self, name: &str, args: &[Value<'_>]) -> Result<Value<'_>> {
        let symbol = self.intern(name)?;
        // XXX Hmm
        let mut args: Vec<emacs_value> = args.iter().map(|v| v.raw).collect();
        raw_call_value!(self, funcall, symbol.raw, args.len() as isize, args.as_mut_ptr())
    }

    pub fn call<'e, T>(&'e self, name: &str, args: T) -> Result<Value<'_>>
    where
        T: IntoLispArgs<'e>,
    {
        let symbol = self.intern(name)?;
        let mut lisp_args = args.into_lisp_args(self)?;
        let lisp_args: &mut [emacs_value] = lisp_args.borrow_mut();
        let ptr = lisp_args.as_mut_ptr();
        let length = lisp_args.len() as isize;
        raw_call_value!(self, funcall, symbol.raw, length, ptr)
    }
}

unsafe impl IntoLispArgs<'_> for &[Value<'_>] {
    type LispArgs = Vec<emacs_value>;

    fn into_lisp_args(self, _: &Env) -> Result<Self::LispArgs> {
        Ok(self.iter().map(|v| v.raw).collect())
    }
}

unsafe impl<'e, T> IntoLispArgs<'e> for T where T: IntoLisp<'e> {
    type LispArgs = [emacs_value; 1];

    #[inline]
    fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs> {
        Ok([self.into_lisp(env)?.raw])
    }
}

emacs_macros::impl_lisp_args_for_tuples_with_max_arity!(12);

emacs_macros::impl_lisp_args_for_arrays_of_max_length!(12);
