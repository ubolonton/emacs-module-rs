# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.19.0] - 2024-03-16
- Upgraded to Rust 2021 edition.
    - This also raised the minimum supported Rust version to 1.56.
- Added support for Rust's non-zero integer types.

## [0.18.0] - 2021-09-26
- Eliminated the build-time dependency on LLVM by putting the raw Rust bindings in source, instead of generating them at build time. This would also make builds faster.

## [0.17.0] - 2021-03-09
- Added `OnceGlobalRef`, which eases the initialization of static references to long-lived Lisp values.
  + Added `use_symbols!`, which enables module code to use Lisp symbols without repeatedly interning them.
  + Added `define_errors!` and `Env::signal` to simplify the process of defining and signaling custom Lisp errors.
- Raised the minimum supported Rust version to 1.45.
- Added `ResultExt::or_signal` to make it more convenient to convert a Rust error into a Lisp error.
- Remove `ResultExt::unwrap_or_propagate`.

## [0.16.2] - 2021-03-04
- Fixed compilation on `aarch64-apple-darwin` (Apple Silicon).

## [0.16.0] - 2021-01-31
- Replaced `failure` with `anyhow` and `thiserror`. Re-export `anyhow` as `emacs::error::anyhow`.

## [0.15.1] - 2021-01-06
- Fixed compilation errors on new versions of `syn`, which removed the `export` module.

## [0.15.0] - 2020-12-27
- Disabled the [workaround](https://github.com/ubolonton/emacs-module-rs/pull/3) for Emacs's [GC bug #31238](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238) if possible (i.e. in Emacs 27+, where the bug was fixed). This should speed up module code in newer Emacs versions.

## [0.14.1] - 2020-10-14
- Fixed proc macro's hygiene issues, for compatibility with Rust 1.47. Example: `emacs-tree-sitter` [fails to compile](https://github.com/ubolonton/emacs-tree-sitter/issues/62).
For details, see Rust's [release note](https://github.com/rust-lang/rust/blob/1.47.0/RELEASES.md#compatibility-notes) and [PR that introduced this change](https://github.com/rust-lang/rust/pull/73084/).

## [0.14.0] - 2020-04-18
- Reworked `Vector`, making it iterable.
- Changed `Env::vector` to return a `Value`.
- Added `call_unprotected` variants to `Value::call`, `Env::call`, `GlobalRef::call`, to enable certain optimizations.

## [0.13.0] - 2020-03-11
- Added `GlobalRef`, which allows keeping Lisp values around without an `Env`.
- Reduced indirection when calling common built-in subroutines through `Env`.
- Removed `module_init!`, `export_functions!`,  and their aliases.
- Replaced `lazy_static` dependency with `once_cell`.
- Fixed memory leaks caused by the [workaround](https://github.com/ubolonton/emacs-module-rs/pull/3) for Emacs's [GC bug #31238](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238), which caused [issue #2](https://github.com/ubolonton/emacs-module-rs/issues/2).

## [0.12.3] - 2020-02-18
- Added `Value::car`, `Value::cdr`.

## [0.12.2] - 2020-02-17
- Added `Env::cons`.

## [0.12.1] - 2020-01-25
- Added `Env::make_user_ptr`, `Value::get_user_ptr`, `Value::get_user_finalizer`.

## [0.12.0] - 2019-12-04
- Added `Env::make_vector` and `Env::vector` functions.
- Added a default implementation for `Transfer::type_name`. This raised the minimum supported Rust version to 1.38 (for `std::any::type_name()`).
- Added function to copy a Lisp string's content to a buffer `Value::copy_string_contents`.
- Deprecated `unwrap_or_propagate`, and marked it as `unsafe`.

## [0.11.0] - 2019-08-05
- Made `Env::call` a lot more flexible. Also added `Value::call`.
- Added `rust-wrong-type-user-ptr` to `wrong-type-argument` classification.
- Deprecated `module_init!` and `export_functions!`.
- Replaced`IntoLisp` implementation for `AsRef<str>` with separate implementations for `&str` and `&String`.

## [0.10.3] - 2019-07-24
- Made `#[defun]` function signatures display correctly in `help-mode` and `helpful-mode`.

## [0.10.2] - 2019-07-24
- Added `defun_prefix` option to `#[module]`.
- Fixed `#[defun]` not handling raw identifiers correctly.

## [0.10.1] - 2019-07-18
- Made `Vector::get` generic by return type.
- Added `FromLisp` and `IntoLisp` implementations for most integer types, and an optional feature `lossy-integer-conversion` to control their behavior. This allows them to be used in `#[defun]` signatures.
- Made `Env::message` take `AsRef<str>`, not just a `&str`.

## [0.10.0] - 2019-07-17
- Raised the minimum supported Rust version to 1.36 (for `MaybeUninit`).
- Added `Vector` type to represent Lisp's vectors.
- Allowed `Rc` and `Arc` to be embedded in `user-ptr` by marking them as `Transfer`.
- Removed `libc` dependency.
- Removed `Transfer::finalizer`.
- Deprecated `env.is_not_nil(value)` in favor of `value.is_not_nil()`.
- Deprecated `env.eq(value1, value2)` in favor of `value1.eq(value2)`.
- Improved Lisp-to-Rust string conversion's performance by making utf-8 validation optional, behind a feature, `utf-8-validation`.
- Improved Rust-to-Lisp string conversion's performance by not creating a temporary `CString`.
- Fixed a safety bug in which short-lived references were allowed to be embedded in `user-ptr`.

## [0.9.0] - 2019-07-11
- `ResultExt` is now a collection of Emacs-specific extension methods for `Result`, instead of a re-export of `failure::ResultExt`.
- Added `failure` as a re-exported sub-mod.
- Added `unwrap_or_propagate` for better panic handling: propagation of non-local exits without `Result`, and improved error messages.

## [0.8.0] - 2019-04-20
- Input parameters with reference types are now interpreted as Rust data structures embedded in `user-ptr` objects.
- Return values are now embedded in `user-ptr` objects if `user_ptr` option is specified.

## [0.7.0] - 2019-04-15
- Greatly improved ergonomics with attribute macros `#[emacs::module]` and `#[defun]`.
- Deprecated macros with `emacs_` prefix.
- Made `Value.env` public.
- Removed the need for user crate to depend directly on `libc`.
- Added lifetime parameter to `FromLisp`.

## [0.6.0] - 2019-03-26
- Upgraded to Rust 2018 edition.

## [0.5.2] - 2018-09-15
- New values obtained from `Env` are now GC-protected. This fixes memory issue [#2](https://github.com/ubolonton/emacs-module-rs/issues/2).

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

[Unreleased]: https://github.com/ubolonton/emacs-module-rs/compare/0.19.0...HEAD
[0.19.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.18.0...0.19.0
[0.18.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.17.0...0.18.0
[0.17.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.16.2...0.17.0
[0.16.2]: https://github.com/ubolonton/emacs-module-rs/compare/0.16.0...0.16.2
[0.16.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.15.1...0.16.0
[0.15.1]: https://github.com/ubolonton/emacs-module-rs/compare/0.15.0...0.15.1
[0.15.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.14.1...0.15.0
[0.14.1]: https://github.com/ubolonton/emacs-module-rs/compare/0.14.0...0.14.1
[0.14.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.13.0...0.14.0
[0.13.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.12.3...0.13.0
[0.12.3]: https://github.com/ubolonton/emacs-module-rs/compare/0.12.2...0.12.3
[0.12.2]: https://github.com/ubolonton/emacs-module-rs/compare/0.12.1...0.12.2
[0.12.1]: https://github.com/ubolonton/emacs-module-rs/compare/0.12.0...0.12.1
[0.12.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.11.0...0.12.0
[0.11.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.10.3...0.11.0
[0.10.3]: https://github.com/ubolonton/emacs-module-rs/compare/0.10.2...0.10.3
[0.10.2]: https://github.com/ubolonton/emacs-module-rs/compare/0.10.1...0.10.2
[0.10.1]: https://github.com/ubolonton/emacs-module-rs/compare/0.10.0...0.10.1
[0.10.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.9.0...0.10.0
[0.9.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.8.0...0.9.0
[0.8.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.5.2...0.6.0
[0.5.2]: https://github.com/ubolonton/emacs-module-rs/compare/0.5.1...0.5.2
[0.5.1]: https://github.com/ubolonton/emacs-module-rs/compare/0.5.0...0.5.1
[0.5.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/ubolonton/emacs-module-rs/compare/bcf0546...0.3.0
[0.2.0]: https://github.com/ubolonton/emacs-module-rs/compare/772bc3b...bcf0546
