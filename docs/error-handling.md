# Error handling architecture

This doc explains how errors move between Lisp, the module layer, and Rust code. It is for maintainers. The user docs are in [`guide/src/errors.md`](guide/src/errors.md). The decisions and their trade-offs are in [ADRs 0001–0003](adrs/README.md).

## Error sources

| Source | Example | Rust | Lisp root |
|---|---|---|---|
| Lisp code, through `funcall` | A Lisp callback signals | `Signal`, `Throw` | As raised |
| Module layer (`emacs-module.c`) | `extract_integer` on a string | `ErrorKind::Module(_)` | `rust-module-error` |
| Rust code in this crate | `String::from_lisp` on bytes that are not UTF-8 | `ErrorKind::Rust(_)` | `rust-error` |

```
module call ─► pending exit ─► classify ─► ErrorKind ─┐
Rust check ─────────────────────────────► ErrorKind ─┼─► propagated with `?` ─► boundary ─► Lisp signal
                                                      └─► handled by Rust code
```

- **Classify:** `Env::handle_module_exit`, whose call-site `rule` has the signature `FnOnce(&Env, Value<'_>) -> Option<ModuleError>`, where the `Value` is the signal symbol ([ADR 0002](adrs/0002-module-signal-classification.md)).
- **Boundary:** `Env::maybe_exit` and `Env::handle_panic` ([ADR 0003](adrs/0003-lisp-signal-mapping.md)).

## Classification: module layer to Rust

`funcall` uses a raw path. Its exits become `Signal` or `Throw` only. The other module calls classify a signal in this order. Only the error path does this.

1. **Call-site rules.** A wrapper owns each symbol that is ambiguous, or whose data changes between versions. Payloads come from the call arguments, not from the signal data.

   | Wrapper | Condition | Variant |
   |---|---|---|
   | `copy_string_contents` | `*len` is larger than the buffer, after the error | `BufferTooSmall` |
   | `vec_get`, `vec_set` | `args-out-of-range`, or `overflow-error` (25) | `IndexOutOfRange` |
   | `extract_integer` (27+), `make_integer` (25, 26) | `overflow-error` | `IntegerOutOfRange` |
   | `extract_integer` | `wrong-type-argument`, any predicate | `WrongType` (`Integer`) |

2. **Generic rule.** For `wrong-type-argument`, read `PRED` and `VALUE` from the data (`car`, `cadr`). Compare `PRED` with symbols cached by `use_symbols!`. A known `PRED` gives `WrongType`, or an encoding variant.
3. **Fallback.** `ModuleError::Signal`, with the original symbol and data.

### Ambiguous symbols

| Symbol | Raised by |
|---|---|
| `overflow-error` | `extract_integer` (27+), `make_integer` (25, 26), `vec_get` and `vec_set` (25, index outside the fixnum range), `make_string` (length), `make_global_ref` (reference count), `funcall` (argument count) |
| `args-out-of-range` | `vec_get`, `vec_set`, `copy_string_contents` (25–30) |

### Emacs version differences

Signal data from the module layer:

| Failure | 25 | 26 | 27–30 | 31+ |
|---|---|---|---|---|
| Buffer too small | `(args-out-of-range)` | `(args-out-of-range)` | `(args-out-of-range ACTUAL REQUIRED MAX)` | `(memory-buffer-too-small ACTUAL REQUIRED)` |
| Index out of range | `(args-out-of-range VECTOR INDEX)` | `(args-out-of-range INDEX 0 LAST)` | Same as 26 | `(args-out-of-range VECTOR INDEX)` |
| Integer out of range | `(overflow-error)` from `make_integer` | Same as 25 | `(overflow-error VALUE)` from `extract_integer` | Same as 27–30 |

Emacs 25 and 26 have no bignums. So `extract_integer` cannot overflow there, but `make_integer` can.

`wrong-type-argument` predicates, for the module functions that this crate wraps:

| `PRED` | Module functions | Emacs |
|---|---|---|
| `integerp`, `floatp`, `stringp`, `vectorp` | `extract_integer`, `extract_float`, `copy_string_contents`, `vec_*` | 25+, except `extract_integer` on 27 |
| `numberp` | `extract_integer` | 27 only |
| `user-ptr` | `get_user_ptr`, `get_user_finalizer` | 25 |
| `user-ptrp` | Same as `user-ptr` | 26+ |
| `unicode-string-p` | `copy_string_contents`, multibyte string with chars outside Unicode | 27+ |
| `utf-8-string-p` | `make_string`, bytes that are not UTF-8 | 28+ |
| `processp`, `pipe-process-p` | `open_channel` | 28+ |

`extract_integer`'s call-site rule handles `wrong-type-argument` itself (see the call-site rules table above), so the generic rule never sees `integerp` or `numberp` from it.

These facts are the same on Emacs 25–32:

- The data of `wrong-type-argument` is `(PRED VALUE)`.
- `copy_string_contents` writes the required size to `*len` before it signals a buffer that is too small.

## Mapping: Rust to Lisp

[ADR 0003](adrs/0003-lisp-signal-mapping.md) gives the rules. These details are not in the ADR:

- `WrongType` builds `PRED` from `LispType`. Thus `LispType::UserPtr` gives `user-ptrp`, also on Emacs 25.
- `IntegerOutOfRange { value: None }` gives the data `()`. `None` means that a Rust value did not fit in Lisp: `u64` to Lisp, or `make_integer` on Emacs 25 and 26.
- `rust-module-buffer-too-small` has the parent `args-out-of-range` on all versions. It also has the parent `memory-buffer-too-small` if Emacs defines that symbol (31+). `Env::define_core_errors` checks `(get 'memory-buffer-too-small 'error-conditions)` at load time. On Emacs 25–30, the message changes from "Args out of range" to "Memory buffer too small".
- `rust-wrong-type-user-ptr` gets the data `(EXPECTED VALUE)`, where `EXPECTED` is the Rust type name.
- `handle_panic` uses the same mapping when the panic payload is an `ErrorKind`.
- `ModuleError::Signal` is raised again as-is. It keeps its standard symbol, without the module root.

## Verification

The classification depends on details of `emacs-module.c` that change between versions. Run the integration tests on every supported version, `emacs-25` to `emacs-32`. The CI matrix has fewer versions.
