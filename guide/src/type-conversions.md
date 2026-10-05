# Type Conversions

The type `Value` represents Lisp values:
- They can be copied around, but cannot outlive the `Env` they come from.
- They are "proxy values": only useful when converted to Rust values, or used as arguments when calling Lisp functions.

## Converting a Lisp `Value` to Rust

This is enabled for types that implement `FromLisp`. Most built-in types are supported. Note that conversion may fail, so the return type is `Result<T>`.

```rust
let i: i64 = value.into_rust()?; // error if Lisp value is not an integer
let f: f64 = value.into_rust()?; // error if Lisp value is nil

let s = value.into_rust::<String>()?;
let s: Option<&str> = value.into_rust()?; // None if Lisp value is nil
```

It's better to declare input types for `#[defun]` than calling `.into_rust()`, unless delayed conversion is needed.

## Converting a Rust Value to Lisp

This is enabled for types that implement `IntoLisp`. Most built-in types are supported. Note that conversion may fail, so the return type is `Result<Value<'_>>`.

```rust
"abc".into_lisp(env)?;
"a\0bc".into_lisp(env)?;

5.into_lisp(env)?;
65.3.into_lisp(env)?;

().into_lisp(env)?; // nil
true.into_lisp(env)?; // t
false.into_lisp(env)?; // nil
```

It's better to declare return type for `#[defun]` than calling `.into_lisp(env)`, whenever possible.

## Integers

Integer conversion is lossless by default. Rust code signals `rust-integer-out-of-range` (`RustError::IntegerOutOfRange`) when a value does not fit the target type, in cases such as:
- A `#[defun]` expecting `u8` gets passed `-1`.
- A `#[defun]` returning `u64` returns a value larger than `i64::max_value()`.

On Emacs 25 and 26, which have no bignums, the module layer itself can reject a Rust integer that does not fit a fixnum. This signals `rust-module-integer-out-of-range` (`ModuleError::IntegerOutOfRange`) instead.

The reverse also happens. On Emacs 27+, which support bignums, a Lisp integer outside the `i64` range signals `rust-module-integer-out-of-range` (`ModuleError::IntegerOutOfRange`), for every target type, for example `i8` or `u64`. This happens even with `lossy-integer-conversion`: extracting the `i64` itself fails, before any Rust-side narrowing runs.

To disable Rust-side narrowing checks, use the `lossy-integer-conversion` feature:

```toml
[dependencies.emacs]
features = ["lossy-integer-conversion"]
```

Support for Rust's `NonZero` integer types is disabled by default. To enable it, use the `nonzero-integer-conversion` feature:
```toml
[dependencies.emacs]
features = ["nonzero-integer-conversion"]
```

## Strings

Lisp strings are converted into Rust `String` structs.

- Unibyte strings:
    - The Lisp side copies the raw bytes directly.
    - The Rust side decodes the bytes, signaling `rust-invalid-utf-8` (`RustError::InvalidUtf8`) if they are not a valid UTF-8 sequence.
- Multibyte strings:
    - The Lisp side encodes the string into raw bytes and copies them.
    - The Rust side decodes the bytes, doing the same validation as above.
    - Since Emacs's internal coding system is a superset, when the string cannot be encoded via UTF-8:
        - On Emacs 25 and 26, the Lisp side doesn't check for this, so the Rust side signals `rust-invalid-utf-8`.
        - On Emacs 27+, the Lisp side signals `rust-module-non-unicode-string`, so the Rust side's check is redundant.

To squeeze out some performance:
- You can avoid allocating memory for `String` structs, by using `value.copy_string_contents(buffer)` with a large enough buffer.
- You can avoid the redundant Rust side's check using `String::from_utf8_unchecked(value.clone_string_contents()?)`.

## Equality

`Value` implements `PartialEq`, which maps to Lisp's `eq` (identity/pointer equality, not `equal`).

```rust
// Two references to the same interned symbol are eq.
let a = env.intern("hello")?;
let b = env.intern("hello")?;
assert!(a == b);

// Two separately allocated strings with the same content are not eq.
let s1 = "hi".into_lisp(env)?;
let s2 = "hi".into_lisp(env)?;
assert!(s1 != s2);
```

`GlobalRef` and `OnceGlobalRef` implement `PartialEq<Value>` (and vice versa), so you can compare a cached global against an incoming argument without rebinding:

```rust
use emacs::use_symbols;

use_symbols! { nil }

#[defun]
fn is_nil(v: Value<'_>) -> Result<bool> {
    Ok(v == *nil)
}
```

The old `value.eq(other)` method is deprecated since 0.20.0. Use `==` instead.

## Vectors

Lisp vectors are represented by the type `Vector`, which can be considered a "sub-type" of `Value`.

To construct Lisp vectors, use `env.make_vector` and `env.vector`, which are efficient wrappers of Emacs's built-in subroutines `make-vector` and `vector`.

```rust
env.make_vector(5, ())?;

env.vector([1, 2, 3])?;

env.vector((1, "x", true))?;
```
