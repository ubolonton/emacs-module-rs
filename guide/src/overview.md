# Overview

`emacs-module-rs` provides high-level Rust binding for Emacs's dynamic module support, and tools that make writing modules easier. It currently supports stable Rust, Emacs 25, OS X and Ubuntu.

## Setting up

- Install the Rust toolchain with [rustup](https://www.rustup.rs/).

- Make sure that your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
