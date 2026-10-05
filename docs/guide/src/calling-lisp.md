# Calling Lisp Functions

Frequently-used Lisp functions are exposed as methods on `env`:

```rust
env.intern("defun")?;

env.message("Hello")?;

env.type_of(5.into_lisp(env)?)?;

env.provide("my-module")?;

env.list((1, "str", true))?;
```

To call arbitrary Lisp functions, use `env.call(func, args)`.
- `func` can be:
  + A string identifying a named function in Lisp.
  + Any Lisp-callable `Value` (a symbol with a function assigned, a lambda, a subr). This can also be written as `func.call(args)`.
- `args` can be:
  + An array, or a slice of `Value`.
  + A tuple of different types, each satisfying the `IntoLisp` trait.

```rust
// (list "str" 2)
env.call("list", ("str", 2))?;
```

```rust
let list = env.intern("list")?;
// (symbol-function 'list)
let subr = env.call("symbol-function", [list])?;
// (funcall 'list "str" 2)
env.call(list, ("str", 2))?;
// (funcall (symbol-function 'list) "str" 2)
env.call(subr, ("str", 2))?;
subr.call(("str", 2))?; // Like the above, but shorter.
```

```rust
// (add-hook 'text-mode-hook 'variable-pitch-mode)
env.call("add-hook", [
    env.intern("text-mode-hook")?,
    env.intern("variable-pitch-mode")?,
])?;
```

```rust
#[defun]
fn listify_vec(vector: Vector) -> Result<Value> {
    let mut args = vec![];
    for e in vector {
        args.push(e)
    }
    vector.0.env.call("list", &args)
}
```

## Caching Symbols and Functions

Every call to `env.intern` and every symbol-lookup in `env.call("name", ...)` does a hash-table lookup inside Emacs. For hot paths, cache the result with `use_symbols!` or `use_functions!`.

### `use_symbols!`

`use_symbols!` declares `static` variables of type `&OnceGlobalRef` that hold interned symbol values. The variables are initialized once when the module is loaded.

```rust
use emacs::{defun, use_symbols, Result, Value};

use_symbols! {
    left right center
}

#[defun(mod_in_name = false)]
fn classify(pos: Value<'_>) -> Result<String> {
    if pos == *left {
        Ok("left".to_owned())
    } else if pos == *right {
        Ok("right".to_owned())
    } else if pos == *center {
        Ok("center".to_owned())
    } else {
        Ok("unknown".to_owned())
    }
}
```

The Lisp name for each symbol is derived by replacing `_` with `-`. Use `=> "lisp-name"` to override:

```rust
use_symbols! {
    nil t
    buffer_read_only => "buffer-read-only"
}
```

If the symbol is bound to a function, you can call it via `env.call(symbol_var, args)`. This goes through symbol lookup on each call. Use `use_functions!` to avoid that indirection.

### `use_functions!`

`use_functions!` is like `use_symbols!`, but stores the function object directly (via `indirect-function`). Calls through these variables skip symbol lookup entirely.

```rust
use emacs::{defun, use_functions, Env, Result, Value};

use_functions! {
    message
    string_to_number => "string-to-number"
}

#[defun]
fn greet_parsed(env: &Env, s: String) -> Result<()> {
    let n: i64 = env.call(string_to_number, (s,))?.into_rust()?;
    env.call(message, (format!("Got {}", n),))?;
    Ok(())
}
```

**Trade-off**: `use_functions!` is faster than `use_symbols!` for repeated calls because it skips symbol lookup. However, if the symbol is later rebound to a different function, the cached reference still points to the original function. Use `use_symbols!` when you need to respect runtime rebinding; use `use_functions!` for built-in and primitive functions where rebinding is not expected.

Both macros can be used only once per Rust `mod`. To cover multiple `mod`s, place one invocation in each.
