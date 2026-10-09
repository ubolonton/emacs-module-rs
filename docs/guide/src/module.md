# Declaring a Module

Each dynamic module must have an initialization function, marked by the attribute macro `#[emacs::module]`. The function's type must be `fn(&Env) -> Result<()>`.

In addition, in order to be loadable by Emacs, the module must be declared GPL-compatible.

```rust
{{#include ../examples/module.rs:init}}
```

## Options

- `name`: By default, the name of the feature provided by the module is the crate's name (with `_` replaced by `-`). There is no need to explicitly call `provide` inside the initialization function. This option allows the function's name, or a string, to be used instead.

    ```rust
{{#include ../examples/module_name_fn.rs:example}}
    ```

- `defun_prefix` and `separator`: Function names in Emacs are conventionally prefixed with the feature name followed by `-`. These 2 options allow a different prefix and separator to be used.

    ```rust
{{#include ../examples/module_separator.rs:example}}
    ```

    ```rust
{{#include ../examples/module_defun_prefix.rs:example}}
    ```

- `mod_in_name`: Whether to use Rust's `mod` path to construct [function names](./functions.md#naming). Default to `true`. For example, supposed that the crate is named `parser`, a `#[defun]` named `next_child` inside `mod cursor` will have the Lisp name of `parser-cursor-next-child`. This can also be overridden for each individual function, by an option of the same name on `#[defun]`.

**Note**: Often time, there's no initialization logic needed. A future version of this crate will support putting `#![emacs::module]` on the crate, without having to define a no-op function. See Rust's [issue #54726](https://github.com/rust-lang/rust/issues/54726).
