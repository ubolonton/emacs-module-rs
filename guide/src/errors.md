# Error Handling and Signaling

Emacs Lisp's [error handling mechanism](https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html) uses [non-local exits](https://www.gnu.org/software/emacs/manual/html_node/elisp/Nonlocal-Exits.html). Rust uses `Result` enum. `emacs-module-rs` converts between the 2 at the Rust-Lisp boundaries (more precisely, Rust-C).

The chosen error type is the `Error` struct from [`anyhow` crate](https://github.com/dtolnay/anyhow):

```rust
pub type Result<T> = result::Result<T, anyhow::Error>;
```

## Handling Lisp Errors in Rust

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

## Handling Rust Errors in Lisp

In addition to [standard errors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Errors.html), Rust module functions can signal Rust-specific errors, which can also be handled by `condition-case`:

- `rust-error`: The message is `Rust error`. This covers all generic Rust-originated errors.
- `rust-wrong-type-user-ptr`: The message is `Wrong type user-ptr`. This happens when Rust code is passed a `user-ptr` of a type it's not expecting. It is a sub-type of `rust-error`.
    ```rust
    // May signal if `value` holds a different type of hash map,
    // or is a `user-ptr` defined in a non-Rust module.
    let r: &RefCell<HashMap<String, String>> = value.into_rust()?;
    ```

### Panics

Unwinding from Rust into C is undefined behavior. `emacs-module-rs` prevents that by using `catch_unwind` at the Rust-to-C boundary to convert a panic into a Lisp's signal/throw of the appropriate type:

- Normally the panic is converted into a Lisp's error signal of the type `rust-panic`. Note that it is **not a sub-type** of `rust-error`.
- If the panic value is an `ErrorKind`, it is converted to the corresponding signal/throw, as if a `Result` was returned. This allows propagating Lisp's non-local exits through contexts where `Result` is not appropriate, e.g. callbacks whose types are dictated by 3rd-party libraries, such as `tree-sitter`.
