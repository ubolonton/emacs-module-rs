use std::{
    os,
    any,
    cell::RefCell,
    rc::Rc,
    sync::{Mutex, RwLock, Arc},
};

use emacs_module::emacs_finalizer_function;

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
    /// Returns the name of this type. This is used to report runtime type errors, when a function
    /// expects values of this type, but receives values of a different type instead. The default
    /// implementation defers to the [`type_name`] function in `std::any`, which can be overridden
    /// for better error reporting.
    ///
    /// [`type_name`]: https://doc.rust-lang.org/std/any/fn.type_name.html
    fn type_name() -> &'static str {
        any::type_name::<Self>()
    }

    // TODO: Consider using a wrapper struct to carry the type info, to enable better runtime
    // reporting of type error (and to enable something like `rs-module/type-of`).
}

impl<'a, 'e: 'a, T: Transfer> FromLisp<'e> for &'a T {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        value.get_raw_pointer().map(|r| unsafe { &*r })
    }
}

/// Finalizes an embedded pointer. This is called by the GC when it discards a `user-ptr`.
///
/// This function also serves as a form of runtime type tag, relying on Rust's mono-morphization.
unsafe extern "C" fn finalize<T: Transfer>(ptr: *mut os::raw::c_void) {
    #[cfg(build = "debug")]
    println!("Finalizing {:#?} {}", ptr, T::type_name());
    Box::from_raw(ptr as *mut T);
}

impl<T: Transfer> IntoLisp<'_> for Box<T> {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let raw = Box::into_raw(self);
        let ptr = raw as *mut os::raw::c_void;
        // Safety: self is forgotten by `into_raw`, so it's safe for the GC to take over.
        unsafe { env.make_user_ptr(Some(finalize::<T>), ptr) }
    }
}

macro_rules! enable_transfers {
    ($($name:ident;)*) => {$(
        impl<T: 'static> Transfer for $name<T> {}

        impl<'e, T: 'static> IntoLisp<'e> for $name<T> {
            #[inline]
            fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
                Box::new(self).into_lisp(env)
            }
        }
    )*};
}

enable_transfers! {
    RefCell;
    Mutex;
    RwLock;
    Rc;
    Arc;
}

impl Env {
    /// Creates and returns a `user-ptr` object that wraps the raw pointer `ptr`. When the object is
    /// garbage-collected, `fin` will be called with `ptr` as the only argument. If `fin` is None,
    /// no finalization will be done.
    ///
    /// In general, prefer the `user-ptr` supported provided by the [`defun`] attr macro. Use this
    /// function only for special `user-ptr` types, such as newtypes wrapping opaque pointers.
    ///
    /// # Safety
    ///
    /// The pointer must be valid until the finalizer is called. The finalizer itself must finalize
    /// the pointer in a safe manner.
    ///
    /// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
    #[allow(unused_unsafe)]
    #[inline]
    pub unsafe fn make_user_ptr(&self, fin: emacs_finalizer_function, ptr: *mut os::raw::c_void) -> Result<Value> {
        unsafe_raw_call_value!(self, make_user_ptr, fin, ptr)
    }
}

impl<'e> Value<'e> {
    /// Returns the raw pointer wrapped in this `user-ptr` object.
    ///
    /// In general, prefer the `user-ptr` supported provided by the [`defun`] attr macro. Use this
    /// function only for special `user-ptr` types, such as newtypes wrapping opaque pointers.
    ///
    /// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
    #[inline]
    pub fn get_user_ptr(self) -> Result<*mut os::raw::c_void> {
        unsafe_raw_call!(self.env, get_user_ptr, self.raw)
    }

    /// Returns the finalizer function associated with this `user-ptr` object.
    ///
    /// In general, prefer the `user-ptr` supported provided by the [`defun`] attr macro. Use this
    /// function only for special `user-ptr` types, such as newtypes wrapping opaque pointers.
    ///
    /// [`defun`]: /emacs-macros/*/emacs_macros/attr.defun.html
    #[inline]
    pub fn get_user_finalizer(self) -> Result<emacs_finalizer_function> {
        unsafe_raw_call!(self.env, get_user_finalizer, self.raw)
    }

    pub(crate) fn get_raw_pointer<T: Transfer>(self) -> Result<*mut T> {
        match self.get_user_finalizer()? {
            // TODO: Consider using dynamic dispatch for finalize, and core::any for type checking.
            Some(fin) if fin == finalize::<T> => {
                let ptr = self.get_user_ptr()?;
                Ok(ptr as *mut T)
            }
            _ => {
                let expected = T::type_name();
                Err(ErrorKind::WrongTypeUserPtr { expected }.into())
            }
        }
    }
}
