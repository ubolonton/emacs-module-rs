use std::{
    cell::RefCell,
    ffi::CString,
    mem::MaybeUninit,
    fmt::Debug,
    io::{Write, PipeWriter},
};

use std::sync::OnceLock;

use emacs_module::{emacs_env, emacs_runtime, emacs_value};

use crate::{subr, error, Value, Result, IntoLisp, call::IntoLispArgs, GlobalRef};

/// Whether the Emacs process that loaded this module has fixed [bug #31238], which caused
/// [issue #2]. If it has, the initialization logic will disable the [workaround] of protecting
/// every newly created [`Value`].
///
/// [bug #31238]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238
/// [issue #2]: https://github.com/ubolonton/emacs-module-rs/issues/2
/// [workaround]: https://github.com/ubolonton/emacs-module-rs/pull/3
/// [`Value`]: struct.Value.html
pub static HAS_FIXED_GC_BUG_31238: OnceLock<bool> = OnceLock::new();

/// Main point of interaction with the Lisp runtime.
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
    /// Raw values "rooted" during the lifetime of this `Env`.
    pub(crate) protected: Option<RefCell<Vec<emacs_value>>>,
}

/// Public APIs.
impl Env {
    #[doc(hidden)]
    pub unsafe fn new(raw: *mut emacs_env) -> Self {
        let protected = if *HAS_FIXED_GC_BUG_31238.get().unwrap_or(&false) {
            None
        } else {
            Some(RefCell::new(vec![]))
        };
        Self { raw, protected }
    }

    #[doc(hidden)]
    pub unsafe fn from_runtime(runtime: *mut emacs_runtime) -> Self {
        // SAFETY: This is only called upon module initialization, during which the runtime pointer
        // is valid.
        unsafe {
            let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
            let raw = get_env(runtime);
            Self::new(raw)
        }
    }

    #[doc(hidden)]
    pub fn raw(&self) -> *mut emacs_env {
        self.raw
    }

    // For testing.
    #[doc(hidden)]
    pub unsafe fn free_last_protected(&self) -> Result<()> {
        if let Some(protected) = &self.protected {
            let raw = *protected.borrow().last().unwrap();
            // SAFETY: Protected values were created by `make_global_ref` and have not been freed.
            let gr = unsafe { GlobalRef::from_raw(raw) };
            gr.free(self)?;
        }
        Ok(())
    }

    pub fn intern(&self, name: &str) -> Result<Value<'_>> {
        unsafe_raw_call_value!(self, intern, CString::new(name)?.as_ptr())
    }

    // TODO: Return an enum?
    pub fn type_of<'e>(&'e self, value: Value<'e>) -> Result<Value<'_>> {
        // Safety: Same lifetimes in type signature.
        unsafe_raw_call_value!(self, type_of, value.raw)
    }

    #[deprecated(since = "0.10.0", note = "Please use `value.is_not_nil()` instead")]
    pub fn is_not_nil<'e>(&'e self, value: Value<'e>) -> bool {
        // Safety: Same lifetimes in type signature.
        unsafe_raw_call_no_exit!(self, is_not_nil, value.raw)
    }

    #[deprecated(since = "0.10.0", note = "Please use `==` instead")]
    pub fn eq<'e>(&'e self, a: Value<'e>, b: Value<'e>) -> bool {
        // Safety: value is lifetime-constrained by this env.
        unsafe_raw_call_no_exit!(self, eq, a.raw, b.raw)
    }

    pub fn cons<'e, A, B>(&'e self, car: A, cdr: B) -> Result<Value<'_>> where A: IntoLisp<'e>, B: IntoLisp<'e> {
        self.call(subr::cons, (car, cdr))
    }

    pub fn list<'e, A>(&'e self, args: A) -> Result<Value<'_>> where A: IntoLispArgs<'e> {
        self.call(subr::list, args)
    }

    pub fn provide(&self, name: &str) -> Result<Value<'_>> {
        let name = self.intern(name)?;
        self.call("provide", [name])
    }

    pub fn message<T: AsRef<str>>(&self, text: T) -> Result<Value<'_>> {
        self.call(subr::message, (text.as_ref(),))
    }

    /// Opens a channel to a pipe process, returning a writer.
    ///
    /// The returned writer can be sent to another thread. Data written to it will be received by
    /// the pipe process's filter function in Emacs.
    ///
    /// Requires Emacs 28+.
    #[cfg(all(feature = "emacs-28"))]
    pub fn open_channel<'e>(&'e self, pipe_process: Value<'e>)
        -> Result<impl Write + Debug + Send + Sync + use<>>
    {
        let raw_fd: i32 = unsafe_raw_call!(self, open_channel, pipe_process.raw)?;

        #[cfg(target_os = "windows")]
        {
            use std::os::windows::io::{FromRawHandle, RawHandle};
            // Safety: open_channel returns a CRT fd created by Emacs via _pipe(). This module must
            // be linked against the same CRT as Emacs (MSVCRT), otherwise get_osfhandle will be
            // called against the wrong CRT's fd table and crash. On CI, this is enforced by running
            // cargo build in an msys2 shell so MSYS2 MINGW64 gcc (MSVCRT) takes precedence over the
            // pre-installed UCRT gcc.
            let handle = unsafe { libc::get_osfhandle(raw_fd) as RawHandle };
            Ok(unsafe { PipeWriter::from_raw_handle(handle) })
        }

        #[cfg(not(target_os = "windows"))]
        {
            use std::os::unix::io::FromRawFd;
            Ok(unsafe { PipeWriter::from_raw_fd(raw_fd) })
        }
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
                error::SIGNAL => unsafe { self.non_local_exit_signal(symbol.assume_init(), data.assume_init()); }
                error::THROW => unsafe { self.non_local_exit_throw(symbol.assume_init(), data.assume_init()); }
                _ => ()
            }
        }
    }
}
