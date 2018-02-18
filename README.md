# Emacs Module in Rust #

This provides a high-level binding to `emacs-module`. Cargo package: [emacs](https://crates.io/crates/emacs/).

## Documentation

- [User Guide](https://ubolonton.github.io/emacs-module-rs/)
- [API Docs](https://docs.rs/emacs/)

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
