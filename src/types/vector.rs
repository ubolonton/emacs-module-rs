use std::convert::TryInto;

use super::*;
use crate::{subr, call::IntoLispArgs};

/// A type that represents Lisp vectors. This is a wrapper around [`Value`] that provides
/// vector-specific methods.
///
/// Arguments to #[[`defun`]] having this type will be type-checked. If you want to omit, or delay
/// this type checking, use [`Value`] instead.
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
///     Ok(Vector::from_value_unchecked(value, 0))
/// }
/// ```
///
/// [`Value`]: struct.Value.html
/// [`defun`]: attr.defun.html
#[derive(Debug, Clone, Copy)]
pub struct Vector<'e> {
    value: Value<'e>,
    len: usize,
}

impl<'e> Vector<'e> {
    #[doc(hidden)]
    #[inline]
    pub fn from_value_unchecked(value: Value<'e>, len: usize) -> Self {
        Self { value, len }
    }

    pub fn get<T: FromLisp<'e>>(&self, i: usize) -> Result<T> {
        let v = self.value;
        let env = v.env;
        // Safety:
        // - Same lifetime.
        // - Emacs does bound checking.
        // - Value doesn't need protection because we are done with it while the vector still lives.
        unsafe_raw_call_value_unprotected!(env, vec_get, v.raw, i as isize)?.into_rust()
    }

    pub fn set<T: IntoLisp<'e>>(&self, i: usize, value: T) -> Result<()> {
        let v = self.value;
        let env = v.env;
        let value = value.into_lisp(env)?;
        // Safety: Same lifetime. Emacs does bound checking.
        unsafe_raw_call!(env, vec_set, v.raw, i as isize, value.raw)
    }

    #[deprecated(since = "0.14.0", note = "Use .len() instead")]
    #[doc(hidden)]
    pub fn size(&self) -> Result<usize> {
        Ok(self.len)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn value(&self) -> Value<'e> {
        self.value
    }
}

impl<'e> FromLisp<'e> for Vector<'e> {
    fn from_lisp(value: Value<'e>) -> Result<Vector<'e>> {
        let env = value.env;
        let len = unsafe_raw_call!(env, vec_size, value.raw)?.try_into()
            .expect("Invalid size from Emacs");
        Ok(Vector { value, len })
    }
}

impl<'e> IntoLisp<'e> for Vector<'e> {
    #[inline(always)]
    fn into_lisp(self, _: &'e Env) -> Result<Value<'_>> {
        Ok(self.value)
    }
}

/// An iterator over the elements of a [`Vector`], as [`Value`] structs.
///
/// [`Vector`]: struct.Vector.html
/// [`Value`]: struct.Value.html
pub struct IntoIter<'e> {
    vector: Vector<'e>,
    i: usize,
}

impl<'e> Iterator for IntoIter<'e> {
    type Item = Value<'e>;

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.i;
        if i >= self.vector.len {
            None
        } else {
            self.i += 1;
            Some(self.vector.get(i).unwrap_or_else(|err| {
                panic!("Unable to get Emacs vector's element at index {}: {}", i, err)
            }))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.vector.len - self.i;
        (remaining, Some(remaining))
    }
}

impl<'e> ExactSizeIterator for IntoIter<'e> {}

impl<'e> IntoIterator for Vector<'e> {
    type Item = Value<'e>;

    type IntoIter = IntoIter<'e>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter { vector: self, i: 0 }
    }
}

impl Env {
    pub fn make_vector<'e, T: IntoLisp<'e>>(&'e self, length: usize, init: T) -> Result<Vector> {
        let value = self.call(subr::make_vector, (length, init))?;
        Ok(Vector::from_value_unchecked(value, length))
    }

    pub fn vector<'e, A: IntoLispArgs<'e>>(&'e self, args: A) -> Result<Value> {
        self.call(subr::vector, args)
    }
}
