# Emacs Module in Rust #

This provides a high-level binding to `emacs-module`. Cargo package: [emacs](https://crates.io/crates/emacs/).

## Documentation

- [User Guide](https://ubolonton.github.io/emacs-module-rs/)
- [API Docs](https://docs.rs/emacs/)

Code for a minimal module looks like this:

```rust
#[macro_use]
extern crate emacs;

use emacs::{Env, Result, Value};

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

fn init(env: &Env) -> Result<Value> {
    fn hello(env: &CallEnv) -> Result<Value> {
        let name: String = env.parse_arg(0)?;
        env.message(&format!("Hello, {}!", name))
    }

    emacs_export_functions! {
        env, "greeting-", {
            "say-hello" => (hello, 1..1)
        }
    }

    env.provide("greeting")
}
```

``` emacs-lisp
(require 'greeting)
(greeting-say-hello "Emacs")
```


## Live Reloading

Emacs does not support unloading modules. Live reloading thus requires a custom module loader. [rs-module](rs-module) is one such loader (which itself is a module that must be loaded by Emacs's normal loading mechanism). See [load.sh](bin/load.sh).

## Sample Modules

[test-module](test-module) uses most of the provided features.

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
