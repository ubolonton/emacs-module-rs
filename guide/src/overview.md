# Overview

`emacs-module-rs` provides high-level Rust binding and tools to write Emacs's dynamic modules. It is easy to use if you know either Rust or Emacs.

It currently supports stable Rust, Emacs 25, OS X and Linux.

## Setting up

- Install the Rust toolchain with [rustup](https://www.rustup.rs/).

- Make sure that your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
