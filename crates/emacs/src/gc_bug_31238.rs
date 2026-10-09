//! Workaround for Emacs GC's [bug #31238], which caused [issue #2]: every newly created [`Value`]
//! is protected by a global reference, freed when its [`Env`] is dropped.
//!
//! The bug was fixed in Emacs 27. The workaround is disabled at load time in Emacs 27+, and it is
//! not compiled with the `emacs-28` feature, because the module cannot load into an older Emacs.
//!
//! [bug #31238]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238
//! [issue #2]: https://github.com/ubolonton/emacs-module-rs/issues/2
//! [`Value`]: crate::Value

use std::{cell::RefCell, mem::MaybeUninit, sync::OnceLock};

use emacs_module::emacs_value;

use crate::{error, Env, Result};

/// Whether the Emacs process that loaded this module has fixed the bug. If it has, the
/// initialization logic disables the [workaround].
///
/// [workaround]: https://github.com/ubolonton/emacs-module-rs/pull/3
static HAS_FIXED_GC_BUG_31238: OnceLock<bool> = OnceLock::new();

/// Raw values "rooted" during the lifetime of an `Env`.
pub(crate) type Protected = RefCell<Vec<emacs_value>>;

fn debugging() -> bool {
    std::env::var("EMACS_MODULE_RS_DEBUG").unwrap_or_default() == "1"
}

pub(crate) fn check(env: &Env) -> Result<()> {
    let version = env.call("default-value", [env.intern("emacs-version")?])?;
    let fixed = env.call("version<=", ("27", version))?.is_not_nil();
    if debugging() {
        env.call("set", (env.intern("module-rs-disable-gc-bug-31238-workaround")?, fixed))?;
    }
    HAS_FIXED_GC_BUG_31238.get_or_init(|| fixed);
    Ok(())
}

pub(crate) fn new_protected() -> Option<Protected> {
    if *HAS_FIXED_GC_BUG_31238.get().unwrap_or(&false) {
        None
    } else {
        Some(RefCell::new(vec![]))
    }
}

impl Env {
    pub(crate) fn protect_raw(&self, raw: emacs_value) {
        if let Some(protected) = &self.protected {
            protected.borrow_mut().push(unsafe_raw_call_no_exit!(self, make_global_ref, raw));
        }
    }

    /// Frees the global reference that protects the last protected value, so that tests can
    /// make [`Drop`] free it a second time.
    ///
    /// # Safety
    ///
    /// The caller must not use the last protected value after this call.
    #[doc(hidden)]
    pub unsafe fn free_last_protected(&self) -> Result<()> {
        if let Some(protected) = &self.protected {
            let raw = *protected.borrow().last().unwrap();
            // Safety: Protected values were created by `make_global_ref` and have not been freed.
            unsafe_raw_call!(self, free_global_ref, raw)?;
        }
        Ok(())
    }
}

// TODO: Add tests to make sure the protected values are not leaked.
impl Drop for Env {
    fn drop(&mut self) {
        if let Some(protected) = &self.protected {
            #[cfg(feature = "debug")]
            println!("Unrooting {} values protected by {:?}", protected.borrow().len(), self);
            // If the `defun` returned a non-local exit, we clear it so that `free_global_ref` doesn't
            // bail out early. Afterwards we restore the non-local exit status and associated data.
            // It's kind of like an `unwind-protect`.
            let mut symbol = MaybeUninit::uninit();
            let mut data = MaybeUninit::uninit();
            // TODO: Check whether calling non_local_exit_check first makes a difference in performance.
            let status = self.non_local_exit_get(&mut symbol, &mut data);
            if status == error::SIGNAL || status == error::THROW {
                self.non_local_exit_clear();
            }
            for raw in protected.borrow().iter() {
                // TODO: Do we want to stop if `free_global_ref` returned a non-local exit?
                // Safety: We assume user code doesn't directly call C function `free_global_ref`.
                unsafe_raw_call_no_exit!(self, free_global_ref, *raw);
            }
            match status {
                error::SIGNAL => unsafe {
                    self.non_local_exit_signal(symbol.assume_init(), data.assume_init());
                },
                error::THROW => unsafe {
                    self.non_local_exit_throw(symbol.assume_init(), data.assume_init());
                },
                _ => (),
            }
        }
    }
}
