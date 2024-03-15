use super::*;

impl FromLisp<'_> for i64 {
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        unsafe_raw_call!(value.env, extract_integer, value.raw)
    }
}

macro_rules! int_from_lisp {
    ($name:ident) => {
        impl FromLisp<'_> for $name {
            #[cfg(not(feature = "lossy-integer-conversion"))]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i: i64 = value.into_rust()?;
                Ok(i.try_into()?)
            }

            #[cfg(feature = "lossy-integer-conversion")]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i: i64 = value.into_rust()?;
                Ok(i as $name)
            }
        }
    }
}

int_from_lisp!(i8);
int_from_lisp!(i16);
int_from_lisp!(i32);
int_from_lisp!(isize);

int_from_lisp!(u8);
int_from_lisp!(u16);
int_from_lisp!(u32);
int_from_lisp!(u64);
int_from_lisp!(usize);

// -------------------------------------------------------------------------------------------------

macro_rules! nonzero_int_from_lisp {
    ($name:ident($primitive:ident)) => {
        impl FromLisp<'_> for std::num::$name {
            #[cfg(not(feature = "lossy-integer-conversion"))]
            fn from_lisp(value: Value<'_>) -> Result<std::num::$name> {
                let i: i64 = value.into_rust()?;
                let i: $primitive = i.try_into()?;
                Ok(i.try_into()?)
            }

            #[cfg(feature = "lossy-integer-conversion")]
            fn from_lisp(value: Value<'_>) -> Result<std::num::$name> {
                let i: i64 = value.into_rust()?;
                let i: $primitive = i as $primitive;
                Ok(i.try_into()?)
            }
        }
    }
}

nonzero_int_from_lisp!(NonZeroU8(u8));
nonzero_int_from_lisp!(NonZeroU16(u16));
nonzero_int_from_lisp!(NonZeroU32(u32));
nonzero_int_from_lisp!(NonZeroU64(u64));
nonzero_int_from_lisp!(NonZeroUsize(usize));

nonzero_int_from_lisp!(NonZeroI8(i8));
nonzero_int_from_lisp!(NonZeroI16(i16));
nonzero_int_from_lisp!(NonZeroI32(i32));
nonzero_int_from_lisp!(NonZeroI64(i64));
nonzero_int_from_lisp!(NonZeroIsize(isize));

// -------------------------------------------------------------------------------------------------

impl IntoLisp<'_> for i64 {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        unsafe_raw_call_value_unprotected!(env, make_integer, self)
    }
}

macro_rules! int_into_lisp {
    ($name:ident) => {
        impl IntoLisp<'_> for $name {
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                (self as i64).into_lisp(env)
            }
        }
    };
    ($name:ident, lossless) => {
        impl IntoLisp<'_> for $name {
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                let i: i64 = self.try_into()?;
                i.into_lisp(env)
            }
        }
    };
}

// Types where `as i64` is lossless.
int_into_lisp!(i8);
int_into_lisp!(i16);
int_into_lisp!(i32);
int_into_lisp!(u8);
int_into_lisp!(u16);
int_into_lisp!(u32);

// Types where `as i64` is lossy.
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(isize);
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(u64);
#[cfg(feature = "lossy-integer-conversion")]
int_into_lisp!(usize);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(isize, lossless);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(u64, lossless);
#[cfg(not(feature = "lossy-integer-conversion"))]
int_into_lisp!(usize, lossless);

// -------------------------------------------------------------------------------------------------

macro_rules! nonzero_int_into_lisp {
    ($name:ident) => {
        #[cfg(feature = "nonzero-integer-conversion")]
        impl IntoLisp<'_> for std::num::$name {
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                self.get().into_lisp(env)
            }
        }
    };
}

nonzero_int_into_lisp!(NonZeroU8);
nonzero_int_into_lisp!(NonZeroU16);
nonzero_int_into_lisp!(NonZeroU32);
nonzero_int_into_lisp!(NonZeroU64);
nonzero_int_into_lisp!(NonZeroUsize);

nonzero_int_into_lisp!(NonZeroI8);
nonzero_int_into_lisp!(NonZeroI16);
nonzero_int_into_lisp!(NonZeroI32);
nonzero_int_into_lisp!(NonZeroI64);
nonzero_int_into_lisp!(NonZeroIsize);
