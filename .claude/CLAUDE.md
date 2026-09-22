# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`emacs-module-rs` is a Rust library providing safe, high-level bindings to Emacs's dynamic module API. It enables extending Emacs with Rust via `#[module]` and `#[defun]` procedural macros.

## Workspace Structure

- **`emacs/`** - Main library crate with public API (type conversions, `Env`, `Value`, error handling)
- **`emacs-macros/`** - Proc macros: `#[module]` and `#[defun]`
- **`emacs-module/`** - Low-level FFI bindings to the C `emacs-module` API; optionally uses `bindgen`
- **`test-module/`** - Integration test suite compiled as `cdylib` and loaded by Emacs
- **`test-module-28/`** - Integration tests for the `emacs-28` feature (`Env::open_channel`, etc.)
- **`rs-module/`** - Live-reloading helper module for development

`test-module` and `test-module-28` are standalone workspaces (own `Cargo.toml` `[workspace]`, own
`Cargo.lock`), not members of the root workspace: they target different minimum Emacs ABI versions,
and Cargo unifies a shared dependency's features across everything resolved together in one
workspace, so keeping them separate stops one's `emacs-N` feature from leaking into the other.

## Build and Test Commands

```bash
# Build all crates
cargo xtask build           # debug
cargo xtask build --release # release

# Run Rust unit tests
cargo test --all

# Run integration tests (requires Emacs installed)
cargo xtask test              # runs once
cargo xtask test --watch      # continuous via cargo-watch
```

The integration tests compile `test-module` as a `.so`/`.dylib`, then run Emacs in batch mode loading `test-module/tests/main.el` (ERT framework).

## Architecture

### Core abstractions

- **`Env`** (`src/env.rs`) - Wraps `*mut emacs_env`; the entry point for all Lisp interaction. Bound by a lifetime to prevent use across GC checkpoints. Contains the workaround for Emacs GC bug #31238.
- **`Value<'e>`** (`src/value.rs`) - A Lisp value bound to the lifetime of its originating `Env`. Converted to Rust types via `into_rust::<T>()`.
- **`FromLisp<'e>` / `IntoLisp<'e>`** (`src/types/`) - Traits for bidirectional type conversion between Lisp and Rust.
- **`Transfer`** (`src/types/user_ptr.rs`) - Trait for embedding Rust structs in Lisp `user-ptr` objects.

### Proc macro pipeline

`#[module]` generates the `emacs_module_init` entry point and registers all `#[defun]`-annotated functions via the `ctor` crate at load time. The registration flow:

1. At link time: `ctor`-attributed stubs push initializers into `__INIT_FNS__`, `__GLOBAL_REFS__`, `__CUSTOM_ERRORS__`
2. At Emacs load time: `emacs_module_init` runs all registered initializers

### Error handling

`Error` / `ErrorKind` in `src/error.rs` wraps both Emacs signals/throws and Rust errors. Use `thiserror` for defining custom Emacs error signals via `define_errors!`.

### Global Lisp values

Use `global_refs!` macro / `OnceGlobalRef` (`src/global.rs`) to hold Lisp values beyond a single `Env` lifetime.

## Key Conventions

- All Lisp-facing names are derived from Rust function names by replacing `_` with `-` (configurable via `defun_prefix`/`separator` on `#[module]`).
- `cdylib` modules must have exactly one `#[module]` annotation in the crate.
- The `user_ptr` option on `#[defun]` selects the concurrency wrapper: `RefCell` (default), `RwLock`, `Mutex`, or `direct`.
- Code is formatted with `rustfmt` (max line width 100, edition 2018 settings — see `.rustfmt.toml`).

## Platform Notes

Platform detection (`.so`/`.dylib`/`.dll`, lib prefix) is handled by `xtask/src/main.rs` via `cfg!()`.

## Changelog

Update `CHANGELOG.md` under `## [Unreleased]` for every non-trivial feature, bug fix, and breaking change. Use the `changelog` skill when writing entries.

## Guide Docs

When writing or editing files under `guide/src/`, use the `guide-writing` skill.
