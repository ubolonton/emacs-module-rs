---
name: guide-writing
description: Use when writing or editing guide docs — files under docs/guide/src/ or docs/guide/examples/
---

# Guide Writing

## Overview

This skill captures the writing style of the `docs/guide/` mdbook. Apply it when creating or modifying any file under `docs/guide/src/`.

## Voice & Tone

- Active voice throughout; imperative for setup steps ("Create a file", "Modify `Cargo.toml`")
- No hedging: "must" not "should ideally", "returns" not "will typically return"
- Semi-formal: engineer-to-engineer, no marketing fluff, no filler transitions

## Sentence & Section Length

- Sentences: 10–25 words; split complex ideas rather than stacking clauses
- Sections: 10–40 lines; one focused idea per section

## Teaching Approach

**Code first, explanation after.** Show a working example, then extract the key point.

```rust
// Show this first, then explain what's happening
#[defun]
fn inc(x: i64) -> Result<i64> {
    Ok(x + 1)
}
```

- Progressive complexity: simple case first, advanced options after
- Explain trade-offs explicitly when presenting multiple options — don't just list them
- Call out failure modes and edge cases directly; don't gloss over them

## Structure

| Content type | Format |
|---|---|
| Reasoning / rationale | Prose |
| Options / parameters | Bullet list (one line each; prose for elaboration) |
| Setup sequences | Numbered steps or sequential bash blocks |
| Complete patterns | Fenced code block |
| Identifiers in prose | Backtick inline code |

## Audience Assumptions

- Readers know Rust **or** Emacs well — no hand-holding for either domain's fundamentals
- C FFI concepts (raw pointers, `extern`, `cdylib`) are understood
- Don't explain what `&mut`, `Box`, or `Result` mean

## Code Examples

- Substantial enough to copy-paste and run
- Include both Rust and Elisp when showing a Lisp-facing API
- Tag Elisp code blocks `elisp`
- Use real scenarios (e.g. wrapping a hash map, a git repo) not toy abstractions
- One good example beats several mediocre ones
- Show the recommended pattern. Do not show a weaker pattern, then suggest a better one in prose
- Do not pin crate versions in snippets. Show `cargo add emacs --features …` instead

### Compiled Rust snippets

Rust code that uses this crate goes in `docs/guide/examples/`, so that it compiles against the current API. Inline Rust blocks are only for code that cannot compile there, such as code that needs a third-party crate.

- Each file is one module (one `#[emacs::module]`), registered as an `[[example]]` in `docs/guide/examples/Cargo.toml`. Set `required-features` for code that needs `emacs-28` or `emacs-32-experimental`.
- Mark the shown part with `// ANCHOR: name` and `// ANCHOR_END: name`. Put the boilerplate that the reader does not need (`plugin_is_GPL_compatible!`, a no-op `init`, wrapper functions) outside the anchors.
- Include it with `{{#include ../examples/file.rs:name}}` at column 0, inside a `rust` fence.
- `{{#include}}` does not dedent. Write fragments of function bodies at column 0. Indent snippets inside Markdown list items to the list level. rustfmt is off for this directory.
- Run `mise run guide:check-examples`. It denies warnings, to catch deprecated APIs.

## Terminology

- Precise and consistent: pick one term and use it throughout (e.g. always "type conversion", never "type casting")
- Domain terms — `defun`, `user-ptr`, `GIL`, `signal`, `throw` — used without apology or inline definition

## Common Mistakes

- Passive voice: "The type can be embedded" → "You can embed the type"
- Over-hedging: "Note that you may want to consider using..." → "Use X when Y"
- Listing options without trade-offs: always say when to prefer each
- Leaking project internals: CI matrices, supported OS counts, test version lists — omit unless directly relevant to the reader's setup
- Linking to maintainer docs: `docs/research/` and `docs/adrs/` are for maintainers. Do not link to them. Put the facts that users need in the guide
