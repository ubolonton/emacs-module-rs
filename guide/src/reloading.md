# Live Reloading

Live code reloading is very useful during development. However, Emacs does not support unloading modules. Live reloading thus requires a custom module loader, e.g. [emacs-rs-module](https://crates.io/crates/emacs-rs-module), which is itself a dynamic module.

To use it, load it in Emacs:

```emacs-lisp
(require 'rs-module)
```

Then use it to load other modules instead of `require` or `module-load`:

```emacs-lisp
;; Will unload the old version of the module first.
(rs-module/load "full/path/to/module.so")
```

`cargo` doesn't support installing dynamic libs yet, so you have to include `emacs-rs-module` as a dev dependency to compile it on your own:

```toml
[dev-dependencies]
emacs-rs-module = { version = "0.6.0" }
```

[magit-libgit2](https://github.com/ubolonton/magit-libgit2#interactive-development) is an example of how to set this all up, to have live-reloading on-save.

A future version will have tighter integration with `cargo`.
