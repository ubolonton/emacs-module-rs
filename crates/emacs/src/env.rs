use std::{
    ffi::CString,
    fmt::Debug,
};

use emacs_module::{emacs_env, emacs_runtime};

use crate::{subr, Value, Result, IntoLisp, call::IntoLispArgs};
#[cfg(not(feature = "emacs-28"))]
use crate::gc_bug_31238::{self, Protected};

/// Main point of interaction with the Lisp runtime.
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
    /// Values protected against GC bug 31238. See [`gc_bug_31238`].
    #[cfg(not(feature = "emacs-28"))]
    pub(crate) protected: Option<Protected>,
}

/// Public APIs.
impl Env {
    #[doc(hidden)]
    pub unsafe fn new(raw: *mut emacs_env) -> Self {
        Self {
            raw,
            #[cfg(not(feature = "emacs-28"))]
            protected: gc_bug_31238::new_protected(),
        }
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
    ///
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::WrongType`](crate::ModuleError::WrongType) with [`LispType::Process`](crate::LispType::Process) or [`LispType::PipeProcess`](crate::LispType::PipeProcess) | `rust-module-wrong-type` |
    #[cfg(all(feature = "emacs-28"))]
    pub fn open_channel<'e>(&'e self, pipe_process: Value<'e>)
        -> Result<impl std::io::Write + Debug + Send + Sync + use<>>
    {
        let raw_fd = unsafe_raw_call!(self, open_channel, pipe_process.raw)?;

        use std::io::PipeWriter;

        #[cfg(target_os = "windows")]
        {
            use std::os::windows::io::{FromRawHandle, RawHandle};
            // Safety: open_channel returns a CRT fd created by Emacs via _pipe(). This module must
            // be linked against the same CRT as Emacs (MSVCRT or UCRT), otherwise get_osfhandle
            // will be called against the wrong CRT's fd table and crash. With the GNU toolchain,
            // the gcc used as the linker selects the CRT: MSYS2 MINGW64 links MSVCRT, UCRT64 links
            // UCRT.
            let handle = unsafe { libc::get_osfhandle(raw_fd) as RawHandle };
            // SAFETY: Emacs dup'ed the open file descriptor.
            Ok(unsafe { PipeWriter::from_raw_handle(handle) })
        }

        #[cfg(not(target_os = "windows"))]
        {
            use std::os::unix::io::FromRawFd;
            // SAFETY: Emacs dup'ed the open file descriptor.
            Ok(unsafe { PipeWriter::from_raw_fd(raw_fd) })
        }
    }
}
