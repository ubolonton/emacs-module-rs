---
name: guide-writing
description: Use when writing or editing guide docs — files under guide/src/
---

# Guide Writing

## Overview

This skill captures the writing style of the `guide/` mdbook. Apply it when creating or modifying any file under `guide/src/`.

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
- Use real scenarios (e.g. wrapping a hash map, a git repo) not toy abstractions
- One good example beats several mediocre ones

## Terminology

- Precise and consistent: pick one term and use it throughout (e.g. always "type conversion", never "type casting")
- Domain terms — `defun`, `user-ptr`, `GIL`, `signal`, `throw` — used without apology or inline definition

## Common Mistakes

- Passive voice: "The type can be embedded" → "You can embed the type"
- Over-hedging: "Note that you may want to consider using..." → "Use X when Y"
- Listing options without trade-offs: always say when to prefer each
- Leaking project internals: CI matrices, supported OS counts, test version lists — omit unless directly relevant to the reader's setup
