# Live Reloading

Live code reloading is very useful during development. However, Emacs does not support unloading modules. Live reloading thus requires a custom module loader, e.g. [emacs-rs-module](https://crates.io/crates/emacs-rs-module), which is itself a dynamic module.

To use it, load it in Emacs:

```lisp
(require 'rs-module)
```

Then use it to load other modules instead of `require` or `module-load`:

```lisp
;; Will unload the old version of the module first.
(rs-module/load "full/path/to/module.so")
```

`cargo` doesn't support installing dynamic libs yet, so you have to include `emacs-rs-module` as a dev dependency to compile it on your own:

```toml
[dev-dependencies]
emacs-rs-module = { version = "0.13.0" }
```

[magit-libgit2](https://github.com/ubolonton/magit-libgit2#interactive-development) is an example of how to set this all up, to have live-reloading on-save.

A future version will have tighter integration with `cargo`.

**Notes**:
- It mainly works on Linux, but potentially because Linux's dynamic loading system is unsafe (i.e. ridden with UB traps).
- It doesn't work on macOS 10.13+ (High Sierra and up), because macOS doesn't unload dynamic libraries that use TLS (thread-local storage), for safety reason. See Rust's [issue #28794](https://github.com/rust-lang/rust/issues/28794#issuecomment-368693049).
- It doesn't work on Windows, since loading the dll prevents writing to its file.
