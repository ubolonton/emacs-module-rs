# Writing Functions

You can use the attribute macro `#[defun]` to export Rust functions to the Lisp runtime, so that Lisp code can call them. These functions must have the signature `fn(..) -> Result<T>`, where `T` is any type that implements `IntoLisp`. Arguments must be of types that implement `FromLisp`.

```rust
#[defun]
fn inc(x: i64) -> Result<i64> {
    Ok(x + 1)
}

#[defun]
fn sum(x: f64, y: f64) -> Result<f64> {
    Ok(x + y)
}
```

## Naming

By default, the function's Lisp name has the form `<feature-prefix>[mod-prefix]<base-name>`.
- `feature-prefix` is the feature's name, followed by `-`. This can be customized by the `name` and `separator` [options](./module.md#options) on `#[emacs::module]`.
- `mod-prefix` is constructed from the function's Rust module path (with `_` and `::` replaced by `-`). This can be turned off crate-wide, or for individual function, using the option `mod_in_name`.
- `base-name` is the function's Rust name (with `_` replaced by `-`). This can be overridden with the option `name`.

Examples:

```rust
// Assuming crate's name is `native_parallelism`.

#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> { Ok(()) }

mod shared_state {
    mod thread {
        // Ignore the nested mod's.
        // native-parallelism/make-thread
        #[defun(mod_in_name = false)]
        fn make_thread(name: String) -> Result<Value<'_>> {
            ..
        }
    }

    mod process {
        // native-parallelism/shared-state-process-launch
        #[defun]
        fn launch(name: String) -> Result<Value<'_>> {
            ..
        }

        // Explicitly named, since Rust identifier cannot contain `:`.
        // native-parallelism/process:pool
        #[defun(mod_in_name = false, name = "process:pool")]
        fn pool(name: String, min: i64, max: i64) -> Result<Value<'_>> {
            ..
        }
    }
}
```

## Interacting with the Lisp runtime

To interact with the Lisp runtime, e.g. calling a Lisp function, declare an additional argument of type `&Env`. This argument does not appear in the function's Lisp signature.

```rust
// Assuming crate's name is `misc`.
use std::time::SystemTime;

#[defun]
fn what_time_is_it(env: &Env) -> Result<Value<'_>> {
    env.message(format!("{:#?}", SystemTime::now()))
}
```

```emacs-lisp
;; No argument
(misc-what-time-is-it)
```
