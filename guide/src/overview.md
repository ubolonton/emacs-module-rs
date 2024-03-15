# Overview

`emacs-module-rs` provides high-level Rust binding and tools to write Emacs's dynamic modules. It is easy to use if you know either Rust or Emacs.

It currently supports:
- Stable Rust 1.56+.
- Emacs 25 or above, built with module support.
- macOS, Linux, Windows.

## Setting up

- Install the Rust toolchain with [rustup](https://www.rustup.rs/).
- Make sure that your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
  + On macOS, the recommended installation method is MacPorts (`emacs-app` and `emacs-mac-app`).
  + On Windows, the recommended installation method for older Emacs versions (before 27.2) is `pacman -S mingw-w64-x86_64-emacs` (msys2), as the archives on GNU FTP's server were built without module support.

### Notes

- On Windows, only Rust's `msvc` toolchain was confirmed to work, not the `gnu` toolchain.
- When the optional feature `bindgen` is enabled, the raw binding will be generated from `emacs-module.h` at build time. Therefore you will also need to install `clang`. (This is recommended only for troubleshooting, though.) For example, on Windows:
    ```powershell
    # In Powershell
    scoop install llvm

    $env:LIBCLANG_PATH = "$(scoop prefix llvm)\bin"
    cargo build --all
    ```

## Known Issues

There is a bug (see [issue #1](https://github.com/ubolonton/emacs-module-rs/issues/1)) with Emacs 26 on Linux that prevents it from loading *any dynamic modules* (even those written in C), if:
- Emacs is built without thread support.
- The OS is Ubuntu 16.04 (Xenial).
