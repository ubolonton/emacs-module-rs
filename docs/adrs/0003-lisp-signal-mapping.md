# 0003. Map error kinds to Lisp signals

- Status: Accepted
- Date: 2026-10-03
- Details: [Error handling architecture](../error-handling.md)

## Context

At the Rust-to-Lisp boundary, the crate converts an `Err` into a Lisp signal. Today, `Signal` and `Throw` are raised again, `WrongTypeUserPtr` becomes `rust-wrong-type-user-ptr`, and all other errors become `(rust-error MESSAGE)`. Lisp selects a handler by the signal symbol and its parents (`error-conditions`).

## Drivers

- Lisp callers need the same information as Rust callers ([0001](0001-error-kinds.md)): the failure, and its origin.
- Existing Lisp handlers must continue to work. They catch standard signals such as `wrong-type-argument`, and some read `(cadr err)`.

## Options

1. **Raise the original Emacs signal again.** Nothing changes. But the origin is lost, and the version differences stay.
2. **One symbol per origin, with the failure in the data.** Handlers must parse the data.
3. **One symbol per variant, with two parents (chosen).** This extends the pattern of `rust-wrong-type-user-ptr`.

Root of the module hierarchy: `rust-module-error` won over `module-error`. Emacs owns the `module-` prefix (`module-load-failed`, …), and the Elisp conventions ask for one package prefix.

## Decision

- Each variant gets one symbol. The name is the root prefix plus the variant name in kebab case.
- Each symbol has two parents: the origin root (`rust-error` or `rust-module-error`), and the standard signal for the same failure.
- The data has the shape of the standard parent. The crate builds it from the payload, so it is the same on all Emacs versions.
- Module-layer symbols use the message of the standard parent, so printed errors mostly do not change. Exception: on Emacs 25–30, the too-small-buffer message changes from "Args out of range" to "Memory buffer too small".
- These do not change: `Signal`, `Throw`, errors that are not `ErrorKind`, and `rust-panic`.
- `ModuleError::Signal` is raised again as-is. It keeps its standard symbol, without the module root.

Examples:

| Variant | Symbol | Parents | Data |
|---|---|---|---|
| `Module(WrongType)` | `rust-module-wrong-type` | `rust-module-error`, `wrong-type-argument` | `(integerp "3")` |
| `Rust(InvalidUtf8)` | `rust-invalid-utf-8` | `rust-error`, `wrong-type-argument` | `(utf-8-string-p "\377")` |

## Consequences

- Good: a Lisp caller can catch one failure, one origin, or one standard condition.
- Good: symbols and data are the same on all Emacs versions. The only difference is an extra parent, `memory-buffer-too-small`, on Emacs 31+.
- Good: tests stop parsing message strings for typed failures.
- Bad: the crate adds one global symbol per variant. The `rust-` prefix decreases the risk of a clash.
- Breaking changes that Lisp code can see:
  - `(car err)` changes for classified module-layer errors. For example, `wrong-type-argument` becomes `rust-module-wrong-type`. Code that compares `(car err)` with `eq` breaks. `condition-case` handlers do not break.
  - Rust-layer failures get structured data, not a message string.
  - The data of `rust-wrong-type-user-ptr` changes.
