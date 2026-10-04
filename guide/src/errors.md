# Error Handling and Signaling

Emacs Lisp's [error handling mechanism](https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html) uses [non-local exits](https://www.gnu.org/software/emacs/manual/html_node/elisp/Nonlocal-Exits.html). Rust uses `Result` enum. `emacs-module-rs` converts between the 2 at the Rust-Lisp boundaries (more precisely, Rust-C).

The chosen error type is the `Error` struct from [`anyhow` crate](https://github.com/dtolnay/anyhow):

```rust
pub type Result<T> = result::Result<T, anyhow::Error>;
```

## Handling Lisp Errors in Rust

Lisp code exits in 2 ways: it signals an error, or it throws a value. Calling it (`env.call`, `Value::call`) surfaces these as `ErrorKind::Signal` and `ErrorKind::Throw`.

When calling a Lisp function, it's usually a good idea to propagate signaled errors with the `?` operator, letting higher level (Lisp) code handle them. If you want to handle a specific error, you can use `error.downcast_ref`:

```rust
match env.call("insert", &[some_text]) {
    Err(error) => {
        // Handle `buffer-read-only` error.
        if let Some(Signal { symbol, .. }) = error.downcast_ref::<ErrorKind>() {
            let buffer_read_only = env.intern("buffer-read-only")?;
            // `symbol` is a `TempValue` that must be converted to `Value`.
            let symbol = unsafe { Ok(symbol.value(env)) };
            if env.eq(symbol, buffer_read_only) {
                env.message("This buffer is not writable!")?;
                return Ok(())
            }
        }
        // Propagate other errors.
        Err(error)
    },
    v => v,
}
```

Note the use of `unsafe` to extract the error symbol as a `Value`. The reason is that, `ErrorKind::Signal` is marked `Send+Sync`, for compatibility with `anyhow`, while `Value` is lifetime-bound by `env`. The `unsafe` contract here requires the error being handled (and its `TempValue`) to come from this `env`, not from another thread, or from a global/thread-local storage.

### Catching Values Thrown by Lisp

This is similar to handling Lisp errors. The only difference is `ErrorKind::Throw` being used instead of `ErrorKind::Signal`.

## Handling Module-layer and Rust-layer Errors in Rust

Two more origins exist.
- `ErrorKind::Module`: The module layer (`emacs-module.c`) rejects some calls, for example `extract_integer` on a string.
- `ErrorKind::Rust`: The Rust layer (i.e. this crate) rejects some values, for example converting a `String` from bytes that are not valid UTF-8.

```rust
match error.downcast_ref::<ErrorKind>() {
    Some(ErrorKind::Signal { .. } | ErrorKind::Throw { .. }) => {
        // Lisp code signaled or threw.
    }
    Some(ErrorKind::Module(_)) => {
        // The module layer rejected the call.
    }
    Some(ErrorKind::Rust(_)) => {
        // Rust code in this crate rejected the value.
    }
    None => {
        // Not an `ErrorKind`.
    }
}
```

`ModuleError` and `RustError` are `#[non_exhaustive]`, so a match on their variants needs an `_` arm:

```rust
match error.downcast_ref::<ErrorKind>() {
    Some(ErrorKind::Module(WrongType { expected: LispType::Integer, .. })) => {
        env.message("Expected an integer")?;
    }
    // `ModuleError` and `RustError` can grow new variants in a minor release.
    _ => return Err(error),
}
```

## Signaling Lisp Errors from Rust

The function `env.signal` allows signaling a Lisp error from Rust code. The error symbol must have been defined, e.g. by the macro `define_errors!`:

```rust
// The parentheses denote parent error signals.
// If unspecified, the parent error signal is `error`.
emacs::define_errors! {
    my_custom_error "This number should not be negative" (arith_error range_error)
}

#[defun]
fn signal_if_negative(env: &Env, x: i16) -> Result<()> {
    if (x < 0) {
        return env.signal(my_custom_error, ("associated", "DATA", 7))
    }
    Ok(())
}
```

## Handling Module-layer and Rust-layer Errors in Lisp

Instead of handling the module-layer and Rust-layer errors above, Rust module functions can propagate them to Lisp. When that happens, the error is "wrapped" in a way that is compatible with using `condition-case` on [standard errors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Errors.html).
- The error symbol has 2 parent symbols: the origin symbol and the standard symbol.
    - For module-layer errors, the origin symbol is `rust-module-error`, and the standard symbol is the original symbol that `emacs-module.c` uses.
    - For Rust-layer errors, the origin symbol is `rust-error`.
- The signal data has the same shape as the standard symbol's data shape in Emacs 31. For example, `rust-module-wrong-type` data is `(integerp "3")`, like `wrong-type-argument`.

`Signal`, `Throw`, and `ModuleError::Signal` are raised again unchanged. Lisp code sees the original symbol and data. `ModuleError::Signal` has no `rust-` symbol.

```
ErrorKind
├── Signal, Throw            from Lisp code
├── Module(ModuleError)      the module layer (emacs-module.c) rejected the call
│   ├── WrongType            expected: LispType
│   ├── NonUnicodeString
│   ├── InvalidUtf8
│   ├── BufferTooSmall
│   ├── IndexOutOfRange
│   ├── IntegerOutOfRange
│   └── Signal               no typed variant
└── Rust(RustError)          this crate's Rust layer rejected the value
    └── WrongTypeUserPtr
```
```
error                                      + standard symbol
├── rust-module-error
│   ├── rust-module-wrong-type             + wrong-type-argument
│   ├── rust-module-non-unicode-string     + wrong-type-argument
│   ├── rust-module-invalid-utf-8          + wrong-type-argument
│   ├── rust-module-buffer-too-small       + args-out-of-range, memory-buffer-too-small (31+)
│   ├── rust-module-index-out-of-range     + args-out-of-range
│   └── rust-module-integer-out-of-range   + overflow-error
├── rust-error
│   └── rust-wrong-type-user-ptr           + wrong-type-argument
└── rust-panic
```

| Variant | Data |
|---|---|
| `WrongType` | `(PREDICATE VALUE)` |
| `BufferTooSmall` | `(ACTUAL REQUIRED)` |
| `IndexOutOfRange` | `(VECTOR INDEX)` |
| `IntegerOutOfRange` | `(VALUE)`, or no data |
| `WrongTypeUserPtr` | `(EXPECTED VALUE)` |

- `EXPECTED` is the Rust type name, not a predicate.
- `PREDICATE` is the Lisp type predicate that the value failed, for example `integerp` or `user-ptrp`. Some, for example `utf-8-string-p`, are not functions.

```rust
// May signal `rust-wrong-type-user-ptr` if `value` holds a different type of hash map,
// or is a `user-ptr` defined in a non-Rust module.
let r: &RefCell<HashMap<String, String>> = value.into_rust()?;
```

### Panics

Unwinding from Rust into C is undefined behavior. `emacs-module-rs` prevents that by using `catch_unwind` at the Rust-to-C boundary to convert a panic into a Lisp's signal/throw of the appropriate type:

- Normally the panic is converted into a Lisp's error signal of the type `rust-panic`. Note that it is **not a sub-type** of `rust-error`.
- If the panic value is an `ErrorKind`, it is converted to the corresponding signal/throw, as if a `Result` was returned. This allows propagating Lisp's non-local exits through contexts where `Result` is not appropriate, e.g. callbacks whose types are dictated by 3rd-party libraries, such as `tree-sitter`.
