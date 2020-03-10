use std::{cell::RefCell, ffi::CString};

use emacs_module::{emacs_runtime, emacs_env, emacs_value};

use crate::{subr, error, Value, Result, IntoLisp, call::IntoLispArgs, GlobalRef};
use std::mem::MaybeUninit;

/// Main point of interaction with the Lisp runtime.
#[derive(Debug)]
pub struct Env {
    pub(crate) raw: *mut emacs_env,
    /// Raw values "rooted" during the lifetime of this `Env`.
    pub(crate) protected: RefCell<Vec<emacs_value>>,
}

/// Public APIs.
impl Env {
    #[doc(hidden)]
    pub unsafe fn new(raw: *mut emacs_env) -> Self {
        let protected = RefCell::new(vec![]);
        Self { raw, protected }
    }

    #[doc(hidden)]
    pub unsafe fn from_runtime(runtime: *mut emacs_runtime) -> Self {
        let get_env = (*runtime).get_environment.expect("Cannot get Emacs environment");
        let raw = get_env(runtime);
        Self::new(raw)
    }

    #[doc(hidden)]
    pub fn raw(&self) -> *mut emacs_env {
        self.raw
    }

    // For testing.
    #[doc(hidden)]
    pub unsafe fn free_last_protected(&self) -> Result<()>{
        let gr = GlobalRef::from_raw(*self.protected.borrow().last().unwrap());
        gr.free(self)?;
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

    #[deprecated(since = "0.10.0", note = "Please use `value1.eq(value2)` instead")]
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
}

// TODO: Add tests to make sure the protected values are not leaked.
impl Drop for Env {
    fn drop(&mut self) {
        #[cfg(build = "debug")]
        println!("Unrooting {} values protected by {:?}", self.protected.borrow().len(), self);
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
        for raw in self.protected.borrow().iter() {
            // TODO: Do we want to stop if `free_global_ref` returned a non-local exit?
            // Safety: We assume user code doesn't directly call C function `free_global_ref`.
            unsafe_raw_call_no_exit!(self, free_global_ref, *raw);
        }
        match status {
            error::SIGNAL => unsafe { self.signal(symbol.assume_init(), data.assume_init()); }
            error::THROW => unsafe { self.throw(symbol.assume_init(), data.assume_init()); }
            _ => ()
        }
    }
}
