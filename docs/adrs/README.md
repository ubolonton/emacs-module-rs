# Architecture decision records

Each record states one decision: its context, the options, the choice, and the consequences. To change a decision, add a new record. Then set the status of the old record to `Superseded by NNNN`.

| ADR | Decision | Status |
|---|---|---|
| [0001](0001-error-kinds.md) | Error kinds: one `ErrorKind`, split by origin | Accepted |
| [0002](0002-module-signal-classification.md) | Classify module-layer signals at the call site | Accepted |
| [0003](0003-lisp-signal-mapping.md) | Map error kinds to Lisp signals | Accepted |
| [0004](0004-scoped-access-to-emacs-memory.md) | Scoped closure for access to memory that Emacs owns | Accepted |
| [0005](0005-unreleased-emacs-abis.md) | Bindings for unreleased Emacs module ABIs | Accepted |
