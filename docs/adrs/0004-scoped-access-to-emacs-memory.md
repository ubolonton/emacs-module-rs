# 0004. Scoped closure for access to memory that Emacs owns

- Status: Accepted
- Date: 2026-10-07
- Details: [Emacs 32 `canvas_data` module API](../research/canvas-data.md)

## Context

Emacs 32 adds `canvas_data`, the first module function that returns a pointer to memory that Emacs owns. Earlier module functions use `emacs_value` handles, copy data into a buffer from the caller, or return the module's own pointer.

The pointer stays valid only while no Lisp code runs. Lisp code can resize the canvas (`xrealloc`), or free it after the spec becomes unreachable. Redisplay reads the buffer. Any `env` call that runs Lisp (`funcall` and similar) can do these things. Thus a safe binding must prevent `env` calls while Rust holds a reference to the buffer.

`Env` is used through `&Env`: defuns receive `&Env`, and every `Value<'e>` holds one. `Value` is `Copy`.

## Drivers

- Safe code must not be able to cause undefined behavior.
- No extra cost: no copy of the buffer, no runtime borrow flag.
- Future APIs of the same kind should use the same pattern.

## Options

1. **Return `&mut [u32]` with the lifetime of `&Env`.** Rejected. `&Env` is a shared reference, so other `&Env` calls still compile while the slice exists.
2. **Require `&mut Env`.** Rejected. Defuns get `&Env`, and each `Value` holds a copy of `&Env`.
3. **Return a guard object.** Rejected. It has the same problem as option 1.
4. **`unsafe` API.** Rejected as the main API. It moves the burden to every caller. It can be added later for measured hot paths.
5. **Scoped closure with a `Send` bound (chosen).**

## Decision

The API calls a closure with the borrowed memory:

```rust
fn with_canvas_data<F, R>(self, f: F) -> Result<R>
where
    F: for<'a> FnOnce(CanvasData<'a>) -> R + Send;
```

- `&Env` and `Value` are not `Send`, because `Env` holds a raw pointer. Thus the closure cannot capture them, and cannot make `env` calls. pyo3's `Python::allow_threads` uses the same technique.
- `for<'a>` stops the return value from holding the borrow.
- The method reads all values that safety needs (for example, the buffer length) before it calls the closure. It does not cache them, because Lisp can change them between calls.

Use this pattern for each future API that gives access to memory that Emacs owns.

## Consequences

- Good: safe code cannot use the memory while Lisp runs, and the check costs nothing at run time.
- Good: the closure can hand data to a background thread, because it is `Send`.
- Bad: the closure cannot capture other values that are not `Send`, for example `Rc` or `RefCell` references. Each API must document this.
- Bad: callers who need several `env` calls between writes must call the method again each time. Each call repeats the lookups (for a canvas: the dimension reads).
