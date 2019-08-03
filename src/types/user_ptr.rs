use std::{
    os,
    cell::RefCell,
    rc::Rc,
    sync::{Mutex, RwLock, Arc},
};

use emacs_module::emacs_value;

use super::*;
use crate::ErrorKind;

/// Allowing a type to be exposed to Lisp, where its values appear as opaque objects, or "embedded
/// user pointers" (printed as `#<user-ptr ...>`).
///
/// When a (boxed) value of this type is transferred to Lisp, the GC becomes its owner. Afterwards,
/// module code can only access it through immutable references.
///
/// The 'static bound disallows transferring short-lived references, which can become invalid while
/// still being held by the Lisp runtime.
///
/// This works, because the returned string is copied into the Lisp runtime.
///
/// ```
/// use emacs::{defun, Result};
///
/// #[defun]
/// fn foo(s: &String) -> Result<&str> {
///     Ok(s)
/// }
/// ```
///
/// This doesn't work, because the function attempts to give the Lisp runtime a temporary reference.
///
/// ```compile_fail
/// use emacs::{defun, Result};
///
/// #[defun(user_ptr)]
/// fn foo(s: &String) -> Result<&str> {
///     Ok(s)
/// }
/// ```
pub trait Transfer: Sized + 'static {
    // TODO: This should be derived automatically. Use `typename` crate or something.
    /// Returns the name of this type. This is used to report runtime type error, when a function
    /// expects this type, but some Lisp code passes a different type of "user pointer".
    fn type_name() -> &'static str;

    // TODO: Consider using a wrapper struct to carry the type info, to enable better runtime
    // reporting of type error (and to enable something like `rs-module/type-of`).
}

impl<'a, 'e: 'a, T: Transfer> FromLisp<'e> for &'a T {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        value.env.get_raw_pointer(value.raw).map(|r| unsafe { &*r })
    }
}

/// Finalizes an embedded pointer. This is called by the GC when it discards a `user-ptr`.
///
/// This function also serves as a form of runtime type tag, relying on Rust's mono-morphization.
unsafe extern "C" fn finalize<T: Transfer>(ptr: *mut os::raw::c_void) {
    #[cfg(build = "debug")]
    println!("Finalizing {} {:#?}", T::type_name(), ptr);
    Box::from_raw(ptr as *mut T);
}

impl<T: Transfer> IntoLisp<'_> for Box<T> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let raw = Box::into_raw(self);
        let ptr = raw as *mut os::raw::c_void;
        raw_call_value!(env, make_user_ptr, Some(finalize::<T>), ptr)
    }
}

enable_transfers! {
    RefCell;
    Mutex;
    RwLock;
    Rc;
    Arc;
}

type Finalizer = unsafe extern "C" fn(ptr: *mut os::raw::c_void);

/// Implementation details.
impl Env {
    pub(crate) fn get_raw_pointer<T: Transfer>(&self, value: emacs_value) -> Result<*mut T> {
        match raw_call!(self, get_user_finalizer, value)? {
            // TODO: Consider using dynamic dispatch for finalize, and core::any for type checking.
            Some::<Finalizer>(fin) if fin == finalize::<T> => {
                let ptr: *mut os::raw::c_void = raw_call!(self, get_user_ptr, value)?;
                Ok(ptr as *mut T)
            }
            _ => {
                let expected = T::type_name();
                Err(ErrorKind::WrongTypeUserPtr { expected }.into())
            }
        }
    }
}
