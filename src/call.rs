use emacs_module::emacs_value;
use emacs_macros::impl_lisp_args_for_tuples_with_max_arity;

use crate::{Env, Value, Result, IntoLisp};

type RawArgs<T> = (*mut emacs_value, usize, T);

// TODO: Seal this trait for safety reasons.
pub trait LispArgs<'e> {
    type Buffer;
    fn into_raw(self, env: &'e Env) -> Result<RawArgs<Self::Buffer>>;
}

impl Env {
    // TODO: Add a convenient macro?
    pub fn call(&self, name: &str, args: &[Value<'_>]) -> Result<Value<'_>> {
        let symbol = self.intern(name)?;
        // XXX Hmm
        let mut args: Vec<emacs_value> = args.iter().map(|v| v.raw).collect();
        raw_call_value!(self, funcall, symbol.raw, args.len() as isize, args.as_mut_ptr())
    }

    pub fn call_flex<'e, T>(&'e self, name: &str, args: T) -> Result<Value<'_>>
    where
        T: LispArgs<'e>,
    {
        let symbol = self.intern(name)?;
        // XXX Hmm
        let (ptr, length, _) = args.into_raw(self)?;
        raw_call_value!(self, funcall, symbol.raw, length as isize, ptr)
    }
}

impl LispArgs<'_> for &[Value<'_>] {
    type Buffer = Vec<emacs_value>;

    fn into_raw(self, env: &Env) -> Result<RawArgs<Self::Buffer>> {
        let mut args: Vec<emacs_value> = self.iter().map(|v| v.raw).collect();
        let raw = (args.as_mut_ptr(), args.len(), args);
        Ok(raw)
    }
}

impl<'e, T> LispArgs<'e> for T where T: IntoLisp<'e> {
    type Buffer = [emacs_value; 1];

    fn into_raw(self, env: &'e Env) -> Result<RawArgs<Self::Buffer>> {
        let mut x = [self.into_lisp(env)?.raw];
        Ok((x.as_mut_ptr(), x.len(), x))
    }
}

//impl LispArgs<'_> for &[Value<'_>; 2] {
//    type Buffer = [emacs_value; 2];
//
//    fn into_raw(self, env: &Env) -> Result<RawArgs<Self::Buffer>> {
//        let mut args = [self[0].raw, self[1].raw];
//        let raw = (args.as_mut_ptr(), args.len(), args);
//        Ok(raw)
//    }
//}

impl_lisp_args_for_tuples_with_max_arity!(12);

mod private {
    pub trait Sealed {}
}