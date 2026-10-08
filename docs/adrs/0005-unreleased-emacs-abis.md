# 0005. Bindings for unreleased Emacs module ABIs

- Status: Accepted
- Date: 2026-10-07

## Context

Users want module functions from the Emacs development branch before the release, for example Emacs 32's `canvas_data`. Until Emacs cuts the release branch, its module ABI can change: `src/module-env-N.h` says that new functions go there until the release. A change can rename, remove, or move a function in `emacs_env`. A binding to the old layout then calls the wrong function pointer.

Released ABIs use `emacs-N` features (for example, `emacs-28`). Each feature selects a header in `emacs-module/include/` and raises the minimum `emacs_env` size that `init.rs` checks at load time.

## Drivers

- Users of a stable feature must not get a breaking change from Emacs development.
- Users who choose the development ABI must know that it can break.
- When Emacs releases version N, the move to `emacs-N` must be easy for users.

## Options

1. **Wait for the Emacs release.** Simple, but users cannot test the API until then, and we get no early feedback for Emacs.
2. **Use `emacs-N` now.** Rejected. The name promises a stable ABI.
3. **`emacs-N-experimental` feature (chosen).**

`-experimental` won over `-devel`: it tells crate users directly that the API can change. `-devel` is Emacs jargon for development builds.

## Decision

- The feature for an unreleased ABI is `emacs-N-experimental`. It enables the previous released feature, for example `emacs-28`.
- The header `emacs-module/include/emacs-module-N.h` records the Emacs commit that it comes from.
- Doc comments of gated APIs say "Experimental: tracks an unreleased module ABI". Changelog entries say the same.
- No CI job is required while CI runners have no build of Emacs N.
- When Emacs N is released:
  1. Compare the released header with the recorded commit, and update the bindings.
  2. Add `emacs-N`.
  3. Keep `emacs-N-experimental = ["emacs-N"]` as an alias for one minor version, then remove it.

## Consequences

- Good: users can try new Emacs module functions early. Stable features do not change.
- Good: the header comment gives a fixed point to compare against when Emacs changes the ABI.
- Bad: if Emacs changes the ABI, a module built with an old crate version can call the wrong function on a newer Emacs N build. The `emacs_env` size check at load time catches only changes in size.
- Bad: one more feature to build and test locally.
