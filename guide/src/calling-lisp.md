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
  + Any Lisp-callable `Value` (a symbol with assigned function, a lambda, a subr).
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
    for i in 0..vector.size()? {
        args.push(vector.get(i)?)
    }
    vector.0.env.call("list", &args)
}
```
