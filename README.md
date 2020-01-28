# Emacs Module in Rust
[![crates.io](https://meritbadge.herokuapp.com/emacs)](https://crates.io/crates/emacs)
[![doc.rs](https://docs.rs/emacs/badge.svg)](https://docs.rs/emacs/)
[![Build Status](https://travis-ci.org/ubolonton/emacs-module-rs.svg?branch=master)](https://travis-ci.org/ubolonton/emacs-module-rs)
[![Build Status](https://dev.azure.com/ubolonton/emacs-module-rs/_apis/build/status/ci?branchName=master&label=build&api-version=6.0-preview.1)](https://dev.azure.com/ubolonton/emacs-module-rs/_build/latest?definitionId=1&branchName=master)

[User Guide](https://ubolonton.github.io/emacs-module-rs/) | [Change Log](https://github.com/ubolonton/emacs-module-rs/blob/master/CHANGELOG.md) | [Examples](https://github.com/ubolonton/emacs-module-rs#example-modules)

This provides a high-level binding to `emacs-module`, Emacs's support for dynamic modules.

Code for a minimal module looks like this:

```rust
use emacs::{defun, Env, Result, Value};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "greeting")]
fn init(_: &Env) -> Result<()> { Ok(()) }

#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}
```

```emacs-lisp
(require 'greeting)
(greeting-say-hello "Emacs")
```

## Example Modules

- [emacs-tree-sitter](https://github.com/ubolonton/emacs-tree-sitter): Binding for tree-sitter, an incremental parsing tool.
- [pullover](https://github.com/ubolonton/pullover): Use Emacs to edit text for other macOS apps.
- [test-module](test-module).
- [emacs-rs-examples](https://github.com/ubolonton/emacs-rs-examples).
- [magit-libgit2](https://github.com/ubolonton/magit-libgit2): Experimental attempt to speed up magit using libgit2.

## Development

- Building:
    ```shell
    bin/build
    ```
- Testing:
    ```shell
    bin/test
    ```
- Continuous testing (requires `cargo-watch`):
    ```shell
    bin/test watch
    ```

On Windows, use PowerShell to run the corresponding `.ps1` scripts.
