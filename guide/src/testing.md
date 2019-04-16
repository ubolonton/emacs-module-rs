# Testing

You can define tests using [ert](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html), then use a bash script to load the module and run the tests. Examples:

- emacs-module-rs's own [tests](https://github.com/ubolonton/emacs-module-rs/blob/master/test-module/tests/main.el) and [script](https://github.com/ubolonton/emacs-module-rs/blob/master/bin/test.sh).
- magit-libgit2's [tests](https://github.com/ubolonton/magit-libgit2/blob/master/elisp/magit-libgit2-test.el) and [script](https://github.com/ubolonton/magit-libgit2/blob/master/bin/test.sh).

Continuous testing during development can be done using `cargo-watch`:

```bash
cargo watch -x 'build --all' -s bin/test.sh
```

A future version will have tighter integration with either `cargo` or [Cask](https://github.com/cask/cask).
