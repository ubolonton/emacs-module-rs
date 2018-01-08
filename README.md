# Emacs Module in Rust #

This provides a high level binding to emacs-module, and some tools that make writing Emacs (dynamic) modules easier.

## Writing a Module ##

- Modify your `Cargo.toml`:
    ```toml
    [lib]
    crate-type = ["cdylib"]

    [dependencies]
    emacs = "0.3.0"
    ```
- Write some code in your `src/lib.rs` following this skeleton.
    ```rust
   #[macro_use]
   extern crate emacs;

   use emacs::{Env, Result, Value};

   emacs_plugin_is_GPL_compatible!();
   emacs_module_init!(init);

   pub fn init(env: &mut Env) -> Result<Value> {
       // Initialization code

       env.provide("my-module")
   }
    ```
- Build the code with `cargo build`
- Create a symlink with `.so` extension
    ```shell
    cd target/debug
    ln -s libmy_module.dylib my-module.so
    ```
- Add `target/debug` to your Emacs's `load-path`.
- Load it in Emacs
    ```emacs-lisp
    (require 'my-module)
    ```

## Live Reloading ##

Emacs does not support unloading modules. Live reloading thus requires a custom module loader. [rs-module](rs-module) is one such loader (which itself is a module that must be loaded by Emacs's normal loading mechanism). See [load.sh](bin/load.sh).

## Sample Modules ##

[test-module](test-module) uses most of the provided features.

## Development ##

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
