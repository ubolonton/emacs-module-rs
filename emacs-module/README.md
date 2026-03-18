Raw FFI for emacs-module.

Modules should use the high-level `emacs` crate instead.

To be self-contained, this crate includes the C header files for supported Emacs versions.

For reproducibility and compilation speed, it includes the generated Rust bindings (a.k.a. code-time generation). Whenever the header files are updated, run this command to update the Rust bindings:
```shell
cargo run -p emacs_module --features bindgen-code
```

For debugging, it also supports build-time generation, which can be enabled via `emacs` crate's `bindgen` feature.
