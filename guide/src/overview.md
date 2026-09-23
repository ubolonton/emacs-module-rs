# Overview

`emacs-module-rs` provides high-level Rust binding and tools to write Emacs's dynamic modules. It is easy to use if you know either Rust or Emacs.

It currently supports:
- Stable Rust (latest stable recommended; 1.87 minimum).
- Emacs 25 or above, built with module support.
- Linux, macOS, FreeBSD, Windows.

## Setting up

- Make sure your Emacs was compiled with module support. Check that `module-file-suffix` is not `nil`, and the function `module-load` is defined.
    - On macOS, the recommended installation method is MacPorts (`emacs-app` and `emacs-mac-app`).
    - On Windows, use either an [official GNU build](https://ftp.gnu.org/gnu/emacs/windows/), or MSYS2's package: `pacman -S mingw-w64-ucrt-x86_64-emacs`.
- Install the Rust toolchain with [rustup](https://www.rustup.rs/).
    - On Windows, use the `stable-gnu` toolchain. Build in the MSYS2 shell that matches the C runtime (CRT) of your Emacs:
        - MINGW64 (`mingw-w64-x86_64-toolchain`) for official GNU builds, which use MSVCRT.
        - UCRT64 (`mingw-w64-ucrt-x86_64-toolchain`) for official `-UCRT64` builds (Emacs 31+) and MSYS2's package, which use UCRT.

### Notes

- When the optional feature `bindgen` is enabled, the raw binding will be generated from `emacs-module.h` at build time. You will also need to install `clang`. (This is recommended only for troubleshooting.) For example, on Windows:
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
