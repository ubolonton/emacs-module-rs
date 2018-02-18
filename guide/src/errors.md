# Error Handling

Emacs Lisp's error handling mechanism uses non-local exits. Rust uses `Result` enum. `emacs-module-rs` converts between the 2 at the Rust-Lisp boundaries (more precisely, Rust-C).

TODO: Expand this section.

## Handling errors from Lisp functions

TODO

## Returning errors to Lisp

TODO

## Panics

Unwinding from Rust into C is undefined behavior. `emacs-module-rs` prevents that by using `catch_unwind` at the Rust-to-C boundary, converting a panic into a Lisp's`signal` of type `'panic`.
