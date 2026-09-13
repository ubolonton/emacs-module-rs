# Re-entering Lisp from a Synchronous Callback

A `defun` runs on the Emacs main thread and holds a live `Env`. Sometimes, still on that thread and still inside that call, native code must call back into Lisp — for example a callback that a foreign runtime (a JNI native, a C library hook) invokes *synchronously* while the `defun` waits for it. That callback has no `Env`, and the `Env`'s lifetime is tied to the `defun`'s stack frame, so it cannot simply be stored and handed over.

The `reentry` module solves this. Register the current call's env at the top of the `defun`, then reconstruct a short-lived `Env` from wherever the callback runs on the same thread.

## Usage

```rust
use emacs::{defun, Env, Result, Value};

#[defun]
fn run_with_callback(env: &Env, arg: Value<'_>) -> Result<Value<'_>> {
    // Register this call's env for the duration of the frame.
    let _guard = env.register_reentry();

    // Call into a foreign runtime that may synchronously call back into Lisp.
    // ... invoke_foreign_runtime(arg) ...

    env.intern("done")
}

// Deeper on the same thread's stack — e.g. an `extern "C"` callback — with no `Env`:
fn on_callback() {
    let result = emacs::reentry::with_current_env(|env| {
        // Convert results to a durable form HERE (see "Value lifetime" below).
        env.call("message", ("called back into Lisp",))
           .and_then(|v| v.into_rust::<String>())
    });
    // `None` if no `defun` is in flight on this thread.
    let _ = result;
}
```

## How It Works

Each `defun` call builds its own `Env`, dropped when the call returns. `register_reentry` pushes that env's raw handle onto a per-thread stack and returns a `ReentryGuard` that pops it on drop. `with_current_env` reconstructs an `Env` from the top of the stack and runs your closure with it, returning `None` if the stack is empty.

Nested `Lisp → native → Lisp → native` calls each register a distinct env, so the stack mirrors the call nesting and `with_current_env` always uses the innermost one. A native reached beneath a `defun` is not itself a module call and creates no env — it re-uses the enclosing `defun`'s env, which is the top of the stack.

## Background Threads

`with_current_env` returns `None` on any thread with no `defun` in flight — including a background thread, where re-entry is impossible because Emacs is single-threaded. A background thread must instead marshal its request to the main thread (for example over an [`open_channel`](./open-channel.md) pipe) and let a `defun` there service it.

## Value Lifetime

The env passed to your closure is dropped as soon as the closure returns. Any `Value` it produces is rooted in that scoped env and freed at that point, so a `Value` must not escape the closure. Convert anything that must outlive the call to a durable form — a `GlobalRef`, or a plain Rust value via `into_rust` — inside the closure.

## Keep the Guard in Place

The registered handle is valid only until the `defun` returns, and `with_current_env`'s soundness relies on the `ReentryGuard` dropping before that. Bind it to a local (`let _guard = ...`) and let it drop in place. Do not `std::mem::forget` it or move it out of the `defun`'s frame: a stale registration would let `with_current_env` reconstruct an env over a dangling handle. Nested guards must drop in reverse order of creation, which stack-scoped locals do automatically.
