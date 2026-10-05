# Testing

You can define tests using [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html), then use a script to load the module and run the tests. Examples:

- emacs-module-rs's own [tests](https://github.com/ubolonton/emacs-module-rs/tree/master/test-module/tests) and [script](https://github.com/ubolonton/emacs-module-rs/blob/master/xtask/src/main.rs).
- emacs-tree-sitter's [tests](https://github.com/ubolonton/emacs-tree-sitter/blob/master/tree-sitter-tests.el) and [script](https://github.com/ubolonton/emacs-tree-sitter/blob/master/bin/test).

For continuous testing during development, use [`cargo-watch`](https://crates.io/crates/cargo-watch) to rebuild the module and rerun the tests on file changes. emacs-module-rs's script wraps it:

```bash
cargo xtask test --watch
```

A future version will have tighter integration with either `cargo` or [Cask](https://github.com/cask/cask).
