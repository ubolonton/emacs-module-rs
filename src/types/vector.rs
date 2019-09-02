use std::convert::TryInto;

use super::*;

/// A type that represents Lisp vectors. This is a newtype wrapper around [`Value`] that provides
/// vector-specific methods.
///
/// Arguments to #[[`defun`]] having this type will be type-checked. This type checking can be
/// omitted by manually wrapping a [`Value`]. Note that Emacs still does type checking when calling
/// methods on the vector.
///
/// ```
/// use emacs::{defun, Value, Vector, Result};
///
/// #[defun]
/// fn must_pass_vector(vector: Vector) -> Result<Vector> {
///     Ok(vector)
/// }
///
/// #[defun]
/// fn no_type_check(value: Value) -> Result<Vector> {
///     Ok(Vector(value))
/// }
/// ```
///
/// [`Value`]: struct.Value.html
/// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
#[derive(Debug, Clone, Copy)]
pub struct Vector<'e>(pub Value<'e>);

impl<'e> Vector<'e> {
    pub fn get<T: FromLisp<'e>>(&self, i: usize) -> Result<T> {
        let v = self.0;
        let env = v.env;
        // Safety: Same lifetime. Emacs does bound checking.
        unsafe_raw_call_value!(env, vec_get, v.raw, i as isize)?.into_rust()
    }

    pub fn set<T: IntoLisp<'e>>(&self, i: usize, value: T) -> Result<()> {
        let v = self.0;
        let env = v.env;
        let value = value.into_lisp(env)?;
        // Safety: Same lifetime. Emacs does bound checking.
        unsafe_raw_call!(env, vec_set, v.raw, i as isize, value.raw)
    }

    pub fn size(&self) -> Result<usize> {
        let v = self.0;
        let env = v.env;
        let result =
            unsafe_raw_call_no_exit!(env, vec_size, v.raw).try_into().expect("invalid size from Emacs");
        env.handle_exit(result)
    }
}

impl<'a, 'e: 'a> FromLisp<'e> for Vector<'a> {
    fn from_lisp(value: Value<'e>) -> Result<Vector<'a>> {
        let vector = Vector(value);
        // TODO: Confirm that this is indeed cheaper than calling vectorp and signaling error.
        vector.size()?;
        Ok(vector)
    }
}

impl<'e> IntoLisp<'e> for Vector<'e> {
    #[inline(always)]
    fn into_lisp(self, _: &'e Env) -> Result<Value<'_>> {
        Ok(self.0)
    }
}
