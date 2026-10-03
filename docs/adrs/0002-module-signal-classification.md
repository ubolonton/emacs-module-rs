# 0002. Classify module-layer signals at the call site

- Status: Accepted
- Date: 2026-10-03
- Details: [Error handling architecture](../error-handling.md)

## Context

The module layer reports every failure as a pending signal: a symbol and data. To create `ErrorKind::Module(_)` ([0001](0001-error-kinds.md)), the crate must select a variant from three inputs: the module function that it called, the symbol, and the data. This has two problems:

- Some symbols are ambiguous. For example, `overflow-error` comes from `extract_integer`, but also from `make_string` (length) and `funcall` (argument count).
- The data shape changes between Emacs versions. For example, an index out of range gives `(args-out-of-range INDEX 0 LAST)` on Emacs 26–30, but `(args-out-of-range VECTOR INDEX)` on 31.

`funcall` uses the same code path as the other module functions, but its signals come from Lisp code.

## Drivers

- Signals from Lisp code must keep their origin.
- Variants and their payloads must not depend on the Emacs version.
- The success path must not become slower.

## Options

1. **A generic rule by symbol, for all module calls.** Ambiguous symbols give wrong variants. Payloads depend on the version.
2. **Call-site rules only.** Every wrapper repeats the rule for `wrong-type-argument`.
3. **Call-site rules, then one generic rule for `wrong-type-argument` (chosen).** Its data, `(PRED VALUE)`, has the same shape on all versions.
4. **Checks in Rust before each call.** These make the success path slower, and can disagree with the Emacs checks.

## Decision

- `funcall` uses a raw path. Its exits become `Signal` or `Throw` only.
- The other module calls classify a signal in this order. Only the error path does this.
  1. **Call-site rules.** Each wrapper knows which signals its function raises. Payloads come from the call arguments. Example: after an error, `copy_string_contents` gives `BufferTooSmall` if `*len` is larger than the buffer. All Emacs versions write the required size there.
  2. **Generic rule.** `wrong-type-argument` with a known `PRED` gives `WrongType`, or an encoding variant (`unicode-string-p` gives `NonUnicodeString`).
  3. **Fallback.** `ModuleError::Signal`, with the original symbol and data.
- Rust code creates `ErrorKind::Rust(_)` where its own check fails.

## Consequences

- Good: the success path does not change. It already checks the exit status.
- Good: payloads are the same on all Emacs versions.
- Good: if a future Emacs changes a signal, the failure becomes `ModuleError::Signal`. It does not become a wrong variant, unless Emacs gives a known symbol a new meaning.
- Bad: the error path adds a few `eq` calls and up to two Lisp calls (`car`, `cadr`). If one of these calls fails, the result is `ModuleError::Signal`.
- Bad: each new module API wrapper must state its call-site rules. A missing rule gives `ModuleError::Signal`, which is safe but not typed.
- Bad: the classification depends on details of `emacs-module.c`, such as predicate names and the `*len` write. Run the integration tests on every supported Emacs version (25–32) to find changes. The CI matrix has fewer versions.
