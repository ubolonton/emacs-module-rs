---
paths:
  - '**/*.rs'

---


# Code Comment and Docstrings

## Docstrings
- Doc (`///`) comments cover *when/why to use*, not just what; use `# Safety`, `# Implementation`, `# Examples` sections; cross-link types with `` [`Type`] `` syntax.

## Unsafe Code

- Every `unsafe fn` must have a `# Safety` doc section explaining the contracts the caller must uphold.
- Every `unsafe {}` block must have a `// Safety:` comment explaining why the usage is sound.
  - If soundness relies on Emacs behavior rather than Rust guarantees, state it explicitly as an assumption.
