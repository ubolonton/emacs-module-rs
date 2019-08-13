# Testing

You can define tests using [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html), then use a bash script to load the module and run the tests. Examples:

- emacs-module-rs's own [tests](https://github.com/ubolonton/emacs-module-rs/blob/master/test-module/tests/main.el) and [script](https://github.com/ubolonton/emacs-module-rs/blob/master/bin/test).
- emacs-tree-sitter's [tests](https://github.com/ubolonton/emacs-tree-sitter/blob/master/tree-sitter-tests.el) and [script](https://github.com/ubolonton/emacs-tree-sitter/blob/master/bin/test).

For continuous testing during development, run this (requires `cargo-watch`):

```bash
bin/test watch
```

A future version will have tighter integration with either `cargo` or [Cask](https://github.com/cask/cask).
