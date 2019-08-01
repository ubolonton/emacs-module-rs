# Declaring a Module

Each dynamic module must have an initialization function, marked by the attribute macro `#[emacs::module]`. The function's type must be `fn(&Env) -> Result<()>`.

In addition, in order to be loadable by Emacs, the module must be declared GPL-compatible.

```rust
emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(env: &Env) -> Result<()> {
    // This is run when Emacs loads the module.
    // More concretely, it is run after all the functions it defines are exported,
    // but before `(provide 'feature-name)` is (automatically) called.
    Ok(())
}
```

## Options

- `name`: By default, the name of the feature provided by the module is the crate's name (with `_` replaced by `-`). There is no need to explicitly call `provide` inside the initialization function. This option allows the function's name, or a string, to be used instead.

    ```rust
    // Putting `rs` in crate's name is discouraged so we use the function's name
    // instead. The feature will be `rs-module-helper`.
    #[emacs::module(name(fn))]
    fn rs_module_helper(_: &Env) -> Result<()> { Ok(()) }
    ```

- `defun_prefix` and `separator`: Function names in Emacs are conventionally prefixed with the feature name followed by `-`. These 2 options allow a different prefix and separator to be used.

    ```rust
    // Use `/` as the separator that goes after feature name, like some other packages.
    #[emacs::module(separator = "/")]
    fn init(_: &Env) -> Result<()> { Ok(()) }
    ```

    ```rust
    // The whole package contains other Lisp files, so the module is named
    // `tree-sitter-dyn`. But we want functions to be `tree-sitter-something`,
    // not `tree-sitter-dyn-something`.
    #[emacs::module(name = "tree-sitter-dyn", defun_prefix = "tree-sitter")]
    fn init(_: &Env) -> Result<()> { Ok(()) }
    ```

- `mod_in_name`: Whether to use Rust's `mod` path to construct [function names](./functions.md#naming). Default to `true`. For example, supposed that the crate is named `parser`, a `#[defun]` named `next_child` inside `mod cursor` will have the Lisp name of `parser-cursor-next-child`. This can also be overridden for each individual function, by an option of the same name on `#[defun]`.

**Note**: Often time, there's no initialization logic needed. A future version of this crate will support putting `#![emacs::module]` on the crate, without having to define a no-op function. See Rust's [issue #54726](https://github.com/rust-lang/rust/issues/54726).
