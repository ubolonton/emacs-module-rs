# Calling Lisp Functions

Frequently-used Lisp functions are exposed as methods on `env`:

```rust
env.intern("defun")?;

env.message("Hello")?;

env.type_of(5.into_lisp(env)?)?;

env.provide("my-module")?;
```

To call arbitrary Lisp functions, use `env.call(&str, &[Value])`:

```rust
// (list "1" 2)
env.call("list", &[
    "1".into_lisp(env)?,
    2.into_lisp(env)?,
])?;

// (add-hook 'text-mode-hook 'variable-pitch-mode)
env.call("add-hook", &[
    env.intern("text-mode-hook")?,
    env.intern("variable-pitch-mode")?,
])?;
```
