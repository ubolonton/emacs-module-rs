# 0001. Error kinds: one `ErrorKind`, split by origin

- Status: Accepted
- Date: 2026-10-03
- Details: [Error handling architecture](../error-handling.md)

## Context

`emacs::Result<T>` is `Result<T, anyhow::Error>`. `ErrorKind` is the only error type that the crate defines for callers to downcast. It expresses Lisp signals and throws, but not most failures of the crate's own APIs. Examples:

| Failure | Rust caller gets | Lisp caller gets |
|---|---|---|
| Unibyte string is not valid UTF-8 | `FromUtf8Error` | `rust-error` + message |
| Buffer too small for `copy_string_contents` | `Signal` | `args-out-of-range` or `memory-buffer-too-small`, by Emacs version |
| `i64` does not fit in `u8` | `TryFromIntError` | `rust-error` + message |

To handle such failures, Rust callers compare symbols through `unsafe`, and Lisp callers parse message strings.

Failures come from 3 sources: Lisp code that the module calls, the module layer (`emacs-module.c`), and Rust code in this crate.

## Drivers

- Callers must be able to handle each failure of an API.
- Callers must be able to tell whether the module layer or Rust code rejected a value. These are different faults. For example, `make_string` rejects bytes from Rust, but `String::from_lisp` rejects bytes from Lisp.
- The convenience of one `Result` type must stay.

## Options

`?` converts any std error into `anyhow::Error`, so all options keep one `Result` type. They differ in the number of types that a caller must try with `downcast_ref`.

1. **One flat enum (chosen)**, like `std::io::ErrorKind`. Callers use one downcast target. But most APIs use few variants, and only the docs show which.
2. **One enum per API area** (`StringError`, `IntegerError`, …). Some failures, such as wrong type, occur in all areas. So one call can fail with 2 types, and callers must try both.
3. **One exact error type per API** (`FromLisp::Error`). Signatures show the failures. But this is the largest break, and the type disappears at the first `?` into `emacs::Result`.

Inside option 1:

- **Origin:** nested enums (`Module(_)`, `Rust(_)`) won over flat variants with a prefix. "All errors of one origin" is then a pattern, not a method call.
- **Wrong type:** `WrongType { expected: LispType }` won over one variant per type, and over the raw predicate symbol. "Any wrong type" stays one pattern. Callers need no `unsafe`, and do not see that Emacs 25 says `user-ptr` where later versions say `user-ptrp`.
- **Exhaustiveness:** the top level lists origins, which is a closed set. Lisp code exits only by signal or throw. New failures go into the nested enums. So only the nested enums are `#[non_exhaustive]`. Also, other crates cannot match a `#[non_exhaustive]` tuple variant as `Module(_)`.
- **Unclassified module-layer signals:** `ModuleError::Signal` won over top-level `Signal`. Top-level `Signal` then means Lisp code only. When a later version classifies such a signal, the change stays inside `ModuleError`.

## Decision

Use one exhaustive `ErrorKind`, with one `#[non_exhaustive]` nested enum per origin. Example (not complete):

```rust
pub enum ErrorKind {
    Signal { symbol: TempValue, data: TempValue }, // Lisp code.
    Throw { tag: TempValue, value: TempValue },    // Lisp code.
    Module(ModuleError), // The module layer rejected the call.
    Rust(RustError),     // Rust code in this crate rejected the value.
}

#[non_exhaustive]
pub enum ModuleError {
    WrongType { expected: LispType, value: TempValue },
    BufferTooSmall { actual: usize, required: usize },
    Signal { symbol: TempValue, data: TempValue }, // No typed variant.
    // …
}

#[non_exhaustive]
pub enum RustError {
    InvalidUtf8 { value: TempValue, source: FromUtf8Error },
    // …
}
```

Key rules:

- `ErrorKind` and its variants are exhaustive. `ModuleError`, `RustError`, their variants, and `LispType` are `#[non_exhaustive]`. Users signal custom errors with `Env::signal`.
- If both origins can detect a failure, the variant has the same name in both enums, for example `IntegerOutOfRange`.
- A module-layer signal with no typed variant becomes `ModuleError::Signal`.
- Each API lists its variants in its `# Errors` docs.

## Consequences

- Good: Rust callers and the Lisp boundary use one downcast target.
- Good: callers match typed failures with no `unsafe` and no version checks.
- Good: one pattern matches all errors of one origin. A match on all origins needs no `_` arm.
- Bad: if Emacs adds a third kind of non-local exit, `ErrorKind` needs a new variant. This is a breaking change.
- Bad: patterns have one more level than in a flat enum.
- Bad: to match one failure from both origins, a caller needs two patterns.
- Bad: signatures do not show the variants. The `# Errors` docs must stay correct.
- Bad: more variants carry `TempValue`. So more code depends on its `unsafe` contract: use a `TempValue` only with the `Env` that it came from. This makes the `FIX` on `TempValue` (make it a rooted value) more important.
- Breaking changes:
  - `ErrorKind::WrongTypeUserPtr` moves into `RustError`.
  - Code that downcasts to `TryFromIntError` or `FromUtf8Error` must downcast to `ErrorKind`. The std error stays available as the `source` of the variant.
  - Exhaustive matches on `ErrorKind` stop compiling.
  - Module-layer failures from APIs other than `funcall` move from `ErrorKind::Signal` to `ErrorKind::Module(_)`. Code that matches `Signal` and compares the symbol still compiles, but no longer matches.
