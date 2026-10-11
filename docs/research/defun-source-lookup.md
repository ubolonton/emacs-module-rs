# Finding the Rust source of module functions

This doc records how `find-function`, xref, and `describe-function` locate a function's
definition, why they cannot locate module functions, and the state of the work on branch
`defun-source`. Sources: Emacs `master` at `1b85386156d`, and tests with Emacs 25–32.

Motivation: emacs-jni (`elisp/jvm.el`) rebuilds `load-history` with an obarray scan, and advises
`find-function-search-for-symbol` with a text search for `#[defun(name = "…")]`. Every module built
with this crate needs the same workaround.

## How Emacs locates a definition

- `find-function` (`find-function-library`), elisp xref (`elisp--xref-find-definitions`), and
  `describe-function` (`find-lisp-object-file-name`) all start with `symbol-file`. If it returns
  nil, they stop. `find-function` signals "Don't know where … is defined".
- Then `find-function-search-for-symbol` opens LIBRARY and searches it with Elisp regexps. If that
  fails, it reads the file as Lisp to expand macros. It has no hook to choose another file.
- `symbol-file` reads `load-history`.

## How `load-history` gets entries

- `defalias` adds `(defun . SYMBOL)` to `current-load-list` (`data.c:979`). `fset` adds nothing.
  `provide` adds `(provide . FEATURE)`.
- `load` binds a new `current-load-list` per file (`lread.c:1034`) and saves it at the end.
  `eval-buffer` does the same.
- `module-load` binds nothing. Entries go to the file that is loading, or are lost if none is.

| Emacs | `load` (or `require`) on a module file |
|---|---|
| 25, 26 | Binds nothing, like `module-load`. Entries go to the enclosing `.el` file. |
| 27+ | Binds a list for the module file (`lread.c:1496-1501`). Entries go to the module file. |

## Recording the source location at compile time

- In an attribute macro, `proc_macro::Span::call_site()` is the attribute's line. The item's first
  token is its first remaining attribute, which may be a docstring above the macro's attribute.
  The item starts at the smaller of the two lines.
- `Span::local_file()` is the path that Cargo passes to rustc. For a workspace member it is relative
  to the workspace root (rustc's cwd), e.g. `crates/foo/src/lib.rs`. `file!()` gives the same path.
  So neither `file!()` nor `file!()` plus `CARGO_MANIFEST_DIR` gives a crate-relative path.
- A proc macro can canonicalize `local_file()` and strip `CARGO_MANIFEST_DIR` (an env var of
  rustc). The result is crate-relative and has no build-machine path.
- `Span::line` and `Span::local_file` are stable since Rust 1.88.

## Design

1. `#[defun]` binds with `defalias`, so `symbol-file` finds module functions.
2. `#[defun]` records `(put SYMBOL 'emacs-module-rs-source '(FILE LINE FEATURE))`. FILE is
   crate-relative. `FEATURE` comes from `init::__FEATURE__`, which `#[module]` sets before the
   exporters run.
3. The package that ships the module sets `(put FEATURE 'emacs-module-rs-source-root DIR)`. The
   crate sets no default: the build directory is not the install directory.
4. `#[module(source_lookup)]` (opt-in) evaluates `crates/emacs/src/source_lookup.el`. It adds a
   named around-advice on `find-function-search-for-symbol`. For a symbol with a recorded location
   and a root, the advice visits FILE at LINE, and ignores LIBRARY. So it also works when the
   entries went to the enclosing `.el` file.

Rejected:
- Advice on `symbol-file`: it breaks the function's contract, and lookup also needs the jump
  advice.
- A separate Lisp package: one more package to release. The embedded code is idempotent.

## Status

| Step | State | Commit |
|---|---|---|
| `defalias` | committed | `80b387d` |
| Source-location property, MSRV 1.88 | committed | `dcf0c9a` |
| `source_lookup` option and advice | committed, with an open regression | `02eca66` |

Tests: `function::symbol-file`, `source::*` (`tests/test-module/tests/source-test.el`), and unit
tests in `crates/emacs-macros/src/source.rs`. Tested only on Emacs 30.

## Open problem: step 1 regresses modules without the advice

With `defalias`, `symbol-file` returns the module file (Emacs 27+) or the enclosing `.el` file
(Emacs 25, 26). A module without `source_lookup`, or without a root, then gets:

| Emacs | Before | After |
|---|---|---|
| 29, 30 | "Don't know where … is defined" | `find-function`/xref open the `.so`, then hang reading it as Lisp |
| 31 | same | "File t.so is large, really open?", then a read error |
| 25, 26, `.el` load | same | opens the `.el` file, "Search failed" |

`describe-function` is not affected, except that its "defined in" link now leads to the same
failure.

Proposed fix (not yet approved):
- Use `defalias` only with `source_lookup`. Other modules keep `fset` and today's behavior. The
  exporter reads a static flag, which `#[module]` sets, like `__MOD_IN_NAME__`.
- In the advice: if the symbol has a recorded location, never fall through to the original. If the
  root is unset or the file is missing, signal an error that names `emacs-module-rs-source-root`.

Rejected: an always-on guard advice in every module. It brings back a global advice that the user
did not opt in to.

## Remaining work

- Apply the fix above, and update the `defalias` changelog entry: it no longer applies to all
  modules.
- Skip `function::symbol-file` on Emacs 25, 26, or assert the enclosing-file behavior there.
- Test on Emacs 25–32.
- Guide: `source_lookup` in `module.md`; source lookup and "load the module with `require`" in
  `functions.md`.
- Changelog entry for `source_lookup`.
- After release: emacs-jni loads the module with `load`, removes its `load-history` rebuild and
  advice, and sets the source root.
