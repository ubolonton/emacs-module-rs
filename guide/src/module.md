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

- `name`: By default, name of the feature provided by the module is the crate's name (with `_` replaced by `-`). There is no need to explicitly call `provide` inside the initialization function. This option allows the function's name, or a string, to be used instead.
- `separator`: Function names in Emacs are conventionally prefixed with the feature name, followed by `-`, this option allows a different separator to be used.
- `mod_in_name`: Whether to put module path in [function names](./functions.md#naming). Default to `true`. This can also be overridden for each individual function, by an option of the same name in `#[defun]`.

```rust
// Putting `rs` in crate's name is discouraged so we use the function's name instead.
// The feature will be `rs-module-helper`.
#[emacs::module(name(fn))]
fn rs_module_helper(_: &Env) -> Result<()> { Ok(()) }
```

```rust
// Use `/` as the separator that goes after feature's name, like some popular packages.
#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> { Ok(()) }
```

**Note**: Often time, there's no initialization logic needed. A future version of this crate will support putting `#![emacs::module]` on the crate, without having to define a no-op function. See Rust's [issue #54726](https://github.com/rust-lang/rust/issues/54726).
