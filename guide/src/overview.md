# Overview

`emacs-module-rs` provides high-level Rust binding and tools to write Emacs's dynamic modules. It is easy to use if you know either Rust or Emacs.

It currently supports stable Rust, Emacs 25/26, macOS/Linux.

## Known Issues

There is a bug (see [issue #1](https://github.com/ubolonton/emacs-module-rs/issues/1)) with Emacs 26 on Linux that prevents it from loading any dynamic modules (even those written in C), if:
- Emacs is built without thread support.
- The OS is Ubuntu 16.04 (Xenial).

## Setting up

- Install the Rust toolchain with [rustup](https://www.rustup.rs/).
- Make sure that your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
