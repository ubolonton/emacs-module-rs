use super::*;

impl FromLisp<'_> for i64 {
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        raw_call!(value.env, extract_integer, value.raw)
    }
}

macro_rules! int_from_lisp {
    ($name:ident) => {
        impl FromLisp<'_> for $name {
            #[cfg(not(feature = "lossy-integer-conversion"))]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i = <i64 as FromLisp>::from_lisp(value)?;
                Ok(::std::convert::TryInto::<$name>::try_into(i)?)
            }

            #[cfg(feature = "lossy-integer-conversion")]
            fn from_lisp(value: Value<'_>) -> Result<$name> {
                let i = <i64 as FromLisp>::from_lisp(value)?;
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

impl IntoLisp<'_> for i64 {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        raw_call_value!(env, make_integer, self)
    }
}

macro_rules! int_into_lisp {
    ($name:ident) => {
        impl IntoLisp<'_> for $name {
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                let i = self as i64;
                i.into_lisp(env)
            }
        }
    };
    ($name:ident, lossless) => {
        impl IntoLisp<'_> for $name {
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                let i = ::std::convert::TryInto::<i64>::try_into(self)?;
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
