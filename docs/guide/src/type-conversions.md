# Type Conversions

The type `Value` represents Lisp values:
- They can be copied around, but cannot outlive the `Env` they come from.
- They are "proxy values": only useful when converted to Rust values, or used as arguments when calling Lisp functions.

## Converting a Lisp `Value` to Rust

This is enabled for types that implement `FromLisp`. Most built-in types are supported. Note that conversion may fail, so the return type is `Result<T>`.

```rust
{{#include ../examples/type_conversions.rs:into_rust}}
```

It's better to declare input types for `#[defun]` than calling `.into_rust()`, unless delayed conversion is needed.

## Converting a Rust Value to Lisp

This is enabled for types that implement `IntoLisp`. Most built-in types are supported. Note that conversion may fail, so the return type is `Result<Value<'_>>`.

```rust
{{#include ../examples/type_conversions.rs:into_lisp}}
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

Use `String` for text. Use `Vec<u8>` for binary data, or for text in an encoding other than UTF-8.

```rust
{{#include ../examples/type_conversions.rs:xor_bytes}}
```

Lisp to Rust conversion copies the bytes of a unibyte string, or the UTF-8 encoding of a multibyte string. `String` then validates them as UTF-8. `Vec<u8>` does not.

| Lisp value | `String` | `Vec<u8>` |
|---|---|---|
| `"abc"` (unibyte) | `"abc"` | `b"abc"` |
| `"\377"` (unibyte, not UTF-8) | `rust-invalid-utf-8` | `b"\xff"` |
| `"é"` (multibyte) | `"é"` | `b"\xc3\xa9"` |
| `(string-to-multibyte "\377")` (raw byte, no UTF-8 encoding) | `rust-module-non-unicode-string` † | `rust-module-non-unicode-string` † |
| `5` | `rust-module-wrong-type` | `rust-module-wrong-type` |

† Emacs 25 and 26 do not check this. `String` signals `rust-invalid-utf-8`. `Vec<u8>` gives `b"\xff"`.

Rust to Lisp conversion:
- `&str` and `String` give a multibyte string, even for ASCII text.
- `&[u8]` and `Vec<u8>` give a unibyte string. They need the `emacs-28` feature, because the module API before Emacs 28 cannot make a unibyte string.

Supported byte types: `Vec<u8>` and `Box<[u8]>` from Lisp; `&[u8]`, `&Vec<u8>`, `Vec<u8>`, and `Box<[u8]>` to Lisp. They convert to a Lisp string, not to a vector of integers.

To squeeze out some performance:
- You can avoid allocating memory for `String` structs, by using `value.copy_string_contents(buffer)` with a large enough buffer.
- If you know the string is valid UTF-8, you can skip the Rust-side check with `String::from_utf8_unchecked(value.clone_string_contents()?)`.

## Equality

`Value` implements `PartialEq`, which maps to Lisp's `eq` (identity/pointer equality, not `equal`).

```rust
{{#include ../examples/type_conversions.rs:equality}}
```

`GlobalRef` and `OnceGlobalRef` implement `PartialEq<Value>` (and vice versa), so you can compare a cached global against an incoming argument without rebinding:

```rust
{{#include ../examples/type_conversions.rs:global_equality}}
```

The old `value.eq(other)` method is deprecated since 0.20.0. Use `==` instead.

## Vectors

Lisp vectors are represented by the type `Vector`, which can be considered a "sub-type" of `Value`.

To construct Lisp vectors, use `env.make_vector` and `env.vector`, which are efficient wrappers of Emacs's built-in subroutines `make-vector` and `vector`.

```rust
{{#include ../examples/type_conversions.rs:vectors}}
```
