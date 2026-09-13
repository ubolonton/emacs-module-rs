//! Re-entering Lisp from a thread already inside a live module call.
//!
//! A module function ([`defun`]) runs on the Emacs main thread and holds a
//! live [`Env`]. If, still on that thread and still inside that call, native
//! code must call back into Lisp — for example a callback invoked
//! synchronously by a foreign runtime (a JNI native, a C library hook) that
//! was reached *through* the `defun` — it has no `Env` of its own. The
//! `Env`'s lifetime is tied to the `defun`'s stack frame, so it cannot be
//! stored and handed to the callback.
//!
//! This module records the current call's raw env on a per-thread stack, and
//! reconstructs a short-lived [`Env`] from it on demand:
//!
//! ```no_run
//! use emacs::{defun, Env, Result, Value};
//!
//! // At the top of a `defun`, register its env for the duration of the call.
//! #[defun]
//! fn run_callback(env: &Env) -> Result<Value<'_>> {
//!     let _guard = env.register_reentry();
//!     // ... call into a foreign runtime that may synchronously call back ...
//!     env.intern("done")
//! }
//!
//! // Deeper on the same thread's stack (e.g. a JNI native), with no `Env`:
//! fn on_callback_from_foreign_runtime() {
//!     let done = emacs::reentry::with_current_env(|env| {
//!         // Convert results to a durable form (GlobalRef / Rust value) HERE,
//!         // before this scoped env drops. A `Value` must not escape `f`.
//!         env.call("1+", (1i64,)).and_then(|v| v.into_rust::<i64>())
//!     });
//!     let _ = done; // None if no `defun` is in flight on this thread
//! }
//! ```
//!
//! # Model
//!
//! Each `defun` call already builds its own [`Env`], dropped when the call
//! returns. Nested Lisp→native→Lisp→native calls therefore each register a
//! distinct env; the stack mirrors that nesting and [`with_current_env`] uses
//! the innermost (top) entry. A native reached synchronously beneath a
//! `defun` is *not* itself a module call and creates no env — it re-uses the
//! nearest enclosing `defun`'s env, which is exactly the top of the stack.
//!
//! The reconstructed env is scoped to the [`with_current_env`] closure. Any
//! [`Value`] it produces is rooted in *that* env's own protection list (the
//! [bug #31238] workaround) and freed when the scoped env drops at the end of
//! the closure — so results must be converted to a durable form (a
//! [`GlobalRef`], or a plain Rust value) inside the closure. This makes the
//! scoped env the `Env`-level analogue of a bounded local frame.
//!
//! [`defun`]: macro@crate::defun
//! [`Env`]: crate::Env
//! [`Value`]: crate::Value
//! [`GlobalRef`]: crate::GlobalRef
//! [bug #31238]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238

use std::cell::RefCell;
use std::marker::PhantomData;

use emacs_module::emacs_env;

use crate::Env;

thread_local! {
    /// Raw envs for the in-flight `defun` calls on this thread, innermost
    /// last. Raw pointers (not `&Env`) so there is no lifetime to store; the
    /// validity of each entry is a dynamic-extent invariant upheld by
    /// [`ReentryGuard`].
    static ENV_STACK: RefCell<Vec<*mut emacs_env>> = const { RefCell::new(Vec::new()) };
}

/// Keeps an [`Env`] registered as the current re-entry target for this thread.
///
/// Created by [`Env::register_reentry`]. While it is alive, [`with_current_env`]
/// on the same thread reconstructs an env from it. It pops the registration on
/// drop — including on unwind — so the registration never outlives the call
/// that made it.
///
/// # Must not be leaked
///
/// The pointer this guard registers is valid only until the enclosing `defun`
/// returns. Soundness of [`with_current_env`] relies on the guard's `Drop`
/// running before that happens, which is automatic for a stack-local guard.
/// Do **not** [`std::mem::forget`] it, move it out of the `defun`'s stack
/// frame, or otherwise prevent it from dropping in place: a stale registration
/// would let [`with_current_env`] reconstruct an env over a dangling pointer.
///
/// Guards must drop in reverse order of creation (LIFO), which stack-scoped
/// locals do automatically. The guard is `!Send`: a registration is only
/// meaningful on the thread that created it.
///
/// [`Env`]: crate::Env
#[derive(Debug)]
pub struct ReentryGuard {
    // Makes the guard `!Send`/`!Sync`: the registration is thread-local, and
    // the raw env is only valid on the thread that pushed it.
    _not_send: PhantomData<*const ()>,
}

impl Drop for ReentryGuard {
    fn drop(&mut self) {
        ENV_STACK.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

impl Env {
    /// Registers this env as the current re-entry target for this thread, so
    /// that native code reached synchronously beneath this call can obtain an
    /// env through [`with_current_env`].
    ///
    /// Call this at the top of a [`defun`] and keep the returned
    /// [`ReentryGuard`] alive for the rest of the call (bind it to a local;
    /// see the [module docs] for the full pattern). The registration is
    /// removed when the guard drops.
    ///
    /// [`defun`]: macro@crate::defun
    /// [module docs]: crate::reentry
    pub fn register_reentry(&self) -> ReentryGuard {
        ENV_STACK.with(|stack| {
            stack.borrow_mut().push(self.raw());
        });
        ReentryGuard { _not_send: PhantomData }
    }
}

/// Runs `f` with an [`Env`] reconstructed from the innermost call registered
/// on this thread by [`Env::register_reentry`], returning `Some(f(&env))`.
///
/// Returns `None` if no call is currently registered on this thread — for
/// example when invoked from a background thread, where re-entry is not
/// possible and the caller must fall back to marshalling the request to the
/// main thread.
///
/// The env passed to `f` is short-lived: it is dropped as soon as `f` returns.
/// Convert anything that must outlive the call (a [`Value`]) to a durable form
/// — a [`GlobalRef`] or a plain Rust value — inside `f`. Do not let a `Value`
/// escape the closure.
///
/// [`Env`]: crate::Env
/// [`Value`]: crate::Value
/// [`GlobalRef`]: crate::GlobalRef
pub fn with_current_env<T>(f: impl FnOnce(&Env) -> T) -> Option<T> {
    let raw = ENV_STACK.with(|stack| stack.borrow().last().copied())?;
    // SAFETY: `raw` is the innermost registered call's env. The `ReentryGuard`
    // that pushed it is still alive (its `Drop` pops it, and this reconstruction
    // only happens deeper on the same thread's stack, within that guard's
    // dynamic extent), so the pointer is valid. `raw` came from `Env::raw()` on
    // a real env. The reconstructed env is confined to this function: `f` gets
    // a borrow, and the env is dropped here — freeing anything it rooted.
    let env = unsafe { Env::new(raw) };
    let result = f(&env);
    drop(env);
    Some(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The raw pointer at the top of this thread's stack, if any. Never
    /// dereferenced by the tests — used only to check the stack mechanics
    /// without constructing a live `Env`.
    fn current_raw() -> Option<*mut emacs_env> {
        ENV_STACK.with(|stack| stack.borrow().last().copied())
    }

    fn fake(addr: usize) -> *mut emacs_env {
        addr as *mut emacs_env
    }

    #[test]
    fn empty_stack_yields_none() {
        assert!(current_raw().is_none());
        // `with_current_env` must not construct any env when the stack is empty.
        let ran = with_current_env(|_env| unreachable!("closure ran with no registered env"));
        assert!(ran.is_none());
    }

    #[test]
    fn guard_registers_and_pops_on_drop() {
        ENV_STACK.with(|s| assert!(s.borrow().is_empty()));
        {
            // Push a fake pointer directly, mirroring what `register_reentry`
            // does, without needing a real `Env`. The guard's `Drop` is what
            // is under test.
            ENV_STACK.with(|s| s.borrow_mut().push(fake(0x1000)));
            let _guard = ReentryGuard { _not_send: PhantomData };
            assert_eq!(current_raw(), Some(fake(0x1000)));
        }
        assert!(current_raw().is_none(), "guard did not pop on drop");
    }

    #[test]
    fn nested_registration_is_lifo() {
        let g1 = ReentryGuard { _not_send: PhantomData };
        ENV_STACK.with(|s| s.borrow_mut().push(fake(0x1)));
        let g2 = ReentryGuard { _not_send: PhantomData };
        ENV_STACK.with(|s| s.borrow_mut().push(fake(0x2)));
        assert_eq!(current_raw(), Some(fake(0x2)));
        drop(g2);
        assert_eq!(current_raw(), Some(fake(0x1)));
        drop(g1);
        assert!(current_raw().is_none());
    }

    /// Exercises the reconstruction path. Disabling the GC-bug #31238
    /// workaround makes `Env` a pointer-only wrapper with a no-op `Drop`, so
    /// the fake pointer is never dereferenced.
    #[test]
    fn with_current_env_reconstructs_top_entry() {
        let _ = crate::env::HAS_FIXED_GC_BUG_31238.set(true);
        assert!(with_current_env(|_env| 1).is_none());

        ENV_STACK.with(|s| s.borrow_mut().push(fake(0xABCD)));
        let _guard = ReentryGuard { _not_send: PhantomData };
        let seen = with_current_env(|env| env.raw());
        assert_eq!(seen, Some(fake(0xABCD)));
    }
}
