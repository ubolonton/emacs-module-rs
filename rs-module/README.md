# Emacs Rust Module

This is an Emacs dynamic module that aims to streamline the development of other Emacs dynamic modules, written in Rust.

## Installation
- Build
    ```shell
    cargo build
    ```
- Load the module in Emacs
    ```elisp
    (module-load "/path/to/emacs-rs-module/target/debug/libemacs_rs_module.dylib")
    ```

## Live reloading another module
To be reloadable, the module must export an entry point named `emacs_rs_module_init`. See [test-module](../test-module/src/lib.rs).

Run this in Emacs after each `cargo build` to reload the module:

```emacs-lisp
(rs-module/load "/path/to/my-module/target/debug/libmy_module.dylib")
```

## TODOs
- Add debug facilities.
- Define interface for unloading.
- Report ERT test results to cargo.
