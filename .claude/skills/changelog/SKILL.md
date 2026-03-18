---
name: changelog
description: Use when writing or editing CHANGELOG.md entries
---

# Changelog

## Format

Follows [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) loosely. No `### Added` / `### Fixed` sub-headers — the entry verb carries the category.

```markdown
## [Unreleased]
- Added `Env::open_channel` for pipe-based communication (Emacs 28+).
    - On Windows, this requires linking against the same CRT as Emacs.

## [0.20.0] - 2026-03-04
- Added `use_functions!`, which enables module code to cache references to Lisp functions.
- Implemented `PartialEq` for `Value`, `GlobalRef`, `OnceGlobalRef`.
```

## Entry Style

- **Verb-first, past tense**: Added, Fixed, Removed, Deprecated, Replaced, Made, Raised, Implemented
- **One line per change**; use `+` sub-bullets for closely related details
- **API names in backticks**: `` `Env::open_channel` ``, `` `#[defun]` ``
- **Link issues/PRs/external refs inline** when relevant
- **Include rationale** when non-obvious (e.g. "This also makes builds faster")

## What to Include

- New public API surface
- Bug fixes visible to users
- Breaking changes (called out by verb: "Removed", "Replaced")
- MSRV bumps
- Performance improvements worth noting

## What to Omit

- Internal refactors with no user-facing effect
- Dependency version bumps (unless they affect MSRV or public API)
- CI / build tooling changes

## Reference Links

Every version must have a compare URL at the bottom:

```markdown
[Unreleased]: https://github.com/ubolonton/emacs-module-rs/compare/0.20.0...HEAD
[0.20.0]: https://github.com/ubolonton/emacs-module-rs/compare/0.19.0...0.20.0
```
