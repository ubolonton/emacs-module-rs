# Overview

`emacs-module-rs` provides high-level Rust binding and tools to write Emacs's dynamic modules. It is easy to use if you know either Rust or Emacs.

It currently supports:
- Stable Rust 1.36+.
- Emacs 25 or above, built with module support.
- macOS, Linux, Windows.

## Known Issues

There is a bug (see [issue #1](https://github.com/ubolonton/emacs-module-rs/issues/1)) with Emacs 26 on Linux that prevents it from loading any dynamic modules (even those written in C), if:
- Emacs is built without thread support.
- The OS is Ubuntu 16.04 (Xenial).

## Setting up

- Install the Rust toolchain with [rustup](https://www.rustup.rs/).
- Make sure that your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
  + On macOS, the recommended installation method is MacPorts (`emacs-app` and `emacs-mac-app`).
  + On Windows, the recommended installation is `pacman -S mingw-w64-x86_64-emacs` (msys2), as the version on GNU FTP's server is built without module support.

### Windows
This was only tested with Rust's `msvc` toolchain, not the `gnu` toolchain.

You will also need to install LLVM and set `LIBCLANG_PATH` accordingly.

```powershell
# In Powershell
scoop install llvm

$env:LIBCLANG_PATH = "$(scoop prefix llvm)\bin"
cargo build --all

.\bin\test.ps1
```
