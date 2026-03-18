# Code Comment Rules

- Comment only when code can't express the intent — names and types first.
- Inline (`//`) comments explain *why*, not *what*; full sentences; 1–3 lines.
- Include issue links for bug workarounds (e.g. `// Emacs GC bug #31238`).
- Doc (`///`) comments cover *when/why to use*, not just what; use `# Safety`, `# Implementation`, `# Examples` sections; cross-link types with `` [`Type`] `` syntax.
- TODOs are explicit and include the reason.

## Unsafe Code

- Every `unsafe fn` must have a `# Safety` doc section explaining the contracts the caller must uphold.
- Every `unsafe {}` block must have a `// Safety:` comment explaining why the usage is sound.
  - If soundness relies on Emacs behavior rather than Rust guarantees, state it explicitly as an assumption.
