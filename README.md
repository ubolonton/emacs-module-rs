# Emacs Module in Rust [![crates.io](https://meritbadge.herokuapp.com/emacs)](https://crates.io/crates/emacs) [![doc.rs](https://docs.rs/emacs/badge.svg)](https://docs.rs/emacs/) [![Build Status](https://travis-ci.org/ubolonton/emacs-module-rs.svg?branch=master)](https://travis-ci.org/ubolonton/emacs-module-rs)

[User Guide](https://ubolonton.github.io/emacs-module-rs/) | [Examples](https://github.com/ubolonton/emacs-rs-examples/)

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

## Live Reloading

Emacs does not support unloading modules. Live reloading thus requires a custom module loader. [rs-module](rs-module) is one such loader (which itself is a module that must be loaded by Emacs's normal loading mechanism). See [load.sh](bin/load.sh).

**Note**: This doesn't work on macOS 10.13+ (High Sierra and up). See Rust's [issue #28794](https://github.com/rust-lang/rust/issues/28794#issuecomment-368693049).

## Sample Modules

- [test-module](test-module).
- [emacs-rs-examples](https://github.com/ubolonton/emacs-rs-examples).
- [magit-libgit2](https://github.com/ubolonton/magit-libgit2) is an experimental attempt to speed up magit using libgit2.

## Development

- Building:
    ```shell
    cargo build --all
    ```
- Testing:
    ```shell
    bin/test.sh
    ```
- Continuous testing (requires `cargo-watch`):
    ```shell
    cargo watch -x 'build --all' -s bin/test.sh
    ```
