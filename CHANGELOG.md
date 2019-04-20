# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [0.8.0] - 2019-04-20
- Input parameters with reference types are now interpreted as Rust data structures embedded in `user-ptr` objects.
- Return values are now embedded in `user-ptr` objects if `user_ptr` option is specified.

## [0.7.0] - 2019-04-15
- Greatly improved ergonomics with attribute macros `#[[emacs::module]` and] `#[defun]`.
- Deprecated macros with `emacs_` prefix.
- Made `Value.env` public.
- Removed the need for user crate to depend directly on `libc`.
- Added lifetime parameter to `FromLisp`.

## [0.6.0] - 2019-03-26
- Upgraded to Rust 2018 edition.

## [0.5.2] - 2018-09-15
- New values obtained from `Env` are now GC-protected. This fixes memory issue #2.

## [0.5.1] - 2018-03-03
- Added `FromLisp` implementation for `Option`.
- Fixed `IntoLisp` not working on `Option<&str>`.

## [0.5.0] - 2018-02-24
- Error handling integration with other Rust crates is improved:
  + The exposed error type is now `failure::Error`.
  + `ErrorKind` enum now implements `failure::Fail`.
- Redundant variants of `ErrorKind` were removed.
- Errors in Rust code now signal custom error types to Lisp:
  + `rust-error`
  + `rust-wrong-type-user-ptr`
- Panics are now caught and signaled to Lisp as a special error type: `rust-panic`.
- `env.is_not_nil` and `env.eq` now return `bool` instead of `Result`.

## [0.4.0] - 2018-02-18
- APIs now take `&Env` instead of `&mut Env`.
- `Value`s are no longer passed by reference, and are lifetime-scoped by `Env`.
- Data conversion:
  + Conversion is now simply `.into_rust()` and `.into_lisp(env)`.
  + `RefCell`, `Mutex`, `RwLock` can be directly transferred to Lisp.
  + Many built-in types are now supported.
- Function declaration:
  + Signatures of exportable functions are now simplified to `fn(&CallEnv) -> Result<T>`.
  + `emacs_subrs!` is replaced by `emacs_export_functions!`.
  + `emacs_lambda!` can be used to create Lisp lambdas.
- Panics no longer unwind into C.
- Load/test scripts now work in Linux.

## [0.3.0] - 2018-01-09
- Values of Rust types that implement `Transfer` can be embedded in Lisp objects.
- Wrapper types `Env` and `Value` are now used instead of raw types.

## [0.2.0] - 2018-01-04
New reworked version
