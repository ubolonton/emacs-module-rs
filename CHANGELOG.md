# 0.7.0 (Apr 15, 2019)
- Greatly improved ergonomics with attribute macros `#[emacs::module]` and `#[defun]`.
- Deprecated macros with `emacs_` prefix.
- Made `Value.env` public.
- Removed the need for user crate to depend directly on `libc`.
- Added lifetime parameter to `FromLisp`.

# 0.6.0 (Mar 26, 2019)
- Upgraded to Rust 2018 edition.

# 0.5.2 (Sep 15, 2018)
- New values obtained from `Env` are now GC-protected. This fixes memory issue #2.

# 0.5.1 (Mar 3, 2018)
- Added `FromLisp` implementation for `Option`.
- Fixed `IntoLisp` not working on `Option<&str>`.

# 0.5.0 (Feb 24, 2018)
- Error handling integration with other Rust crates is improved:
  + The exposed error type is now `failure::Error`.
  + `ErrorKind` enum now implements `failure::Fail`.
- Redundant variants of `ErrorKind` were removed.
- Errors in Rust code now signal custom error types to Lisp:
  + `rust-error`
  + `rust-wrong-type-user-ptr`
- Panics are now caught and signaled to Lisp as a special error type: `rust-panic`.
- `env.is_not_nil` and `env.eq` now return `bool` instead of `Result`.

# 0.4.0 (Feb 18, 2018)
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

# 0.3.0 (Jan 9, 2018)
- Values of Rust types that implement `Transfer` can be embedded in Lisp objects.
- Wrapper types `Env` and `Value` are now used instead of raw types.

# 0.2.0 (Jan 4, 2018)
New reworked version
