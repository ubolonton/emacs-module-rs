# Calling Lisp Functions

Frequently-used Lisp functions are exposed as methods on `env`:

```rust
{{#include ../examples/calling_lisp.rs:env_methods}}
```

To call arbitrary Lisp functions, use `env.call(func, args)`.
- `func` can be:
  + A string identifying a named function in Lisp.
  + Any Lisp-callable `Value` (a symbol with a function assigned, a lambda, a subr). This can also be written as `func.call(args)`.
- `args` can be:
  + An array, or a slice of `Value`.
  + A tuple of different types, each satisfying the `IntoLisp` trait.

```rust
{{#include ../examples/calling_lisp.rs:call_by_name}}
```

```rust
{{#include ../examples/calling_lisp.rs:call_value}}
```

```rust
{{#include ../examples/calling_lisp.rs:add_hook}}
```

```rust
{{#include ../examples/calling_lisp.rs:listify_vec}}
```

## Caching Symbols and Functions

Every call to `env.intern` and every symbol-lookup in `env.call("name", ...)` does a hash-table lookup inside Emacs. For hot paths, cache the result with `use_symbols!` or `use_functions!`.

### `use_symbols!`

`use_symbols!` declares `static` variables of type `&OnceGlobalRef` that hold interned symbol values. The variables are initialized once when the module is loaded.

```rust
{{#include ../examples/calling_lisp.rs:use_symbols}}
```

The Lisp name for each symbol is derived by replacing `_` with `-`. Use `=> "lisp-name"` to override:

```rust
{{#include ../examples/calling_lisp.rs:use_symbols_rename}}
```

If the symbol is bound to a function, you can call it via `env.call(symbol_var, args)`. This goes through symbol lookup on each call. Use `use_functions!` to avoid that indirection.

### `use_functions!`

`use_functions!` is like `use_symbols!`, but stores the function object directly (via `indirect-function`). Calls through these variables skip symbol lookup entirely.

```rust
{{#include ../examples/calling_lisp.rs:use_functions}}
```

**Trade-off**: `use_functions!` is faster than `use_symbols!` for repeated calls because it skips symbol lookup. However, if the symbol is later rebound to a different function, the cached reference still points to the original function. Use `use_symbols!` when you need to respect runtime rebinding; use `use_functions!` for built-in and primitive functions where rebinding is not expected.

Both macros can be used only once per Rust `mod`. To cover multiple `mod`s, place one invocation in each.
