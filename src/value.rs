use std::cell::{RefCell, Ref, RefMut};

use emacs_module::emacs_value;

use crate::{Env, Result, FromLisp, Transfer};

/// A type that represents Lisp values.
/// Values of this type can be copied around, but are lifetime-bound to the [`Env`] they come from.
///
/// They are also "proxy values" that are only useful when converted to Rust values, or used as
/// arguments when calling back into the Lisp runtime.
///
/// [`Env`]: struct.Env.html
#[derive(Debug, Clone, Copy)]
pub struct Value<'e> {
    pub(crate) raw: emacs_value,
    pub env: &'e Env,
}

impl<'e> Value<'e> {
    /// Constructs a new `Value`. Module code should not call this directly. It is public only for
    /// some internal macros to use.
    ///
    /// # Safety
    ///
    /// The raw value must come from the given [`Env`].
    ///
    /// [`Env`]: struct.Env.html
    #[doc(hidden)]
    pub unsafe fn new(raw: emacs_value, env: &'e Env) -> Self {
        Self { raw, env }
    }

    /// Constructs a new `Value` and "roots" its underlying raw value (GC-managed) during the
    /// lifetime of the given [`Env`]. Module code should not call this directly. It is public only
    /// for some internal macros to use.
    ///
    /// # Safety
    ///
    /// The raw value must still be alive. This function is needed to protect new values returned
    /// from Emacs runtime, due to [this issue](https://github.com/ubolonton/emacs-module-rs/issues/2).
    ///
    /// [`Env`]: struct.Env.html
    #[allow(unused_unsafe)]
    #[doc(hidden)]
    pub unsafe fn new_protected(raw: emacs_value, env: &'e Env) -> Self {
        env.protected.borrow_mut().push(raw_call_no_exit!(env, make_global_ref, raw));
        Self::new(raw, env)
    }

    pub fn is_not_nil(&self) -> bool {
        let env = self.env;
        raw_call_no_exit!(env, is_not_nil, self.raw)
    }

    // TODO: Decide what we want for PartialEq (==, !=): eq vs. eql vs. equal.
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, other: Value<'e>) -> bool {
        let env = self.env;
        raw_call_no_exit!(env, eq, self.raw, other.raw)
    }

    /// Converts this value into a Rust value of the given type.
    #[inline(always)]
    pub fn into_rust<T: FromLisp<'e>>(self) -> Result<T> {
        FromLisp::from_lisp(self)
    }

    #[inline]
    pub fn into_ref<T: 'static>(self) -> Result<Ref<'e, T>> {
        let container: &RefCell<T> = self.into_rust()?;
        // TODO: Use .borrow(), we want panics.
        Ok(container.try_borrow()?)
    }

    #[inline]
    pub fn into_ref_mut<T: 'static>(self) -> Result<RefMut<'e, T>> {
        let container: &RefCell<T> = self.into_rust()?;
        // TODO: Use .borrow_mut(), we want panics.
        Ok(container.try_borrow_mut()?)
    }

    /// Returns a mutable reference to the underlying Rust data wrapped by this value.
    ///
    /// # Safety
    ///
    /// There are several ways this can go wrong:
    ///
    /// - Lisp code can pass the same object through 2 different values in an argument list.
    /// - Rust code earlier in the call chain may have copied this value.
    /// - Rust code later in the call chain may receive a copy of this value.
    ///
    /// In general, it is better to wrap Rust data in `RefCell`, `Mutex`, or `RwLock` guards, before
    /// moving them to Lisp, and then only access them through these guards (which can be obtained
    /// back through [`into_rust`]). This method is for squeezing out the last bit of performance in
    /// very rare situations.
    ///
    /// [`into_rust`]: #method.into_rust
    pub unsafe fn get_mut<T: Transfer>(&mut self) -> Result<&mut T> {
        self.env.get_raw_pointer(self.raw).map(|r| &mut *r)
    }
}
