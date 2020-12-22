use std::cell::{RefCell, Ref, RefMut};

use emacs_module::emacs_value;

use crate::{subr, Env, Result, FromLisp, Transfer};

/// A type that represents Lisp values.
/// Values of this type can be copied around, but are lifetime-bound to the [`Env`] they come from.
///
/// They are also "proxy values" that are only useful when converted to Rust values, or used as
/// arguments when calling back into the Lisp runtime.
///
/// [`Env`]: struct.Env.html
#[derive(Debug, Clone, Copy)]
pub struct Value<'e> {
    pub raw: emacs_value,
    pub env: &'e Env,
}

impl<'e> Value<'e> {
    /// Constructs a new `Value`. Module code should not call this directly. It is public only for
    /// some internal macros to use.
    ///
    /// # Safety
    ///
    /// The raw value must be valid for as long as the returned `Value` is used. This usually means
    /// "as long as [`Env`] is alive", but can be shorter, depending on how the returned `Value` is
    /// used at call site.
    ///
    /// [`Env`]: struct.Env.html
    #[doc(hidden)]
    pub unsafe fn new(raw: emacs_value, env: &'e Env) -> Self {
        Self { raw, env }
    }

    /// Protects this value by registering with its [`Env`], effectively "rooting" the underlying
    /// Lisp object during the lifetime of the [`Env`].
    ///
    /// Module code should not call this directly. It is public only for certain internal macros and
    /// functions to work around Emacs GC's [bug #31238], which caused [issue #2].
    ///
    /// [`Env`]: struct.Env.html
    /// [bug #31238]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31238
    /// [issue #2]: https://github.com/ubolonton/emacs-module-rs/issues/2
    #[doc(hidden)]
    #[inline]
    pub fn protect(self) -> Self {
        let Self { env, raw } = self;
        env.protected.borrow_mut().push(unsafe_raw_call_no_exit!(env, make_global_ref, raw));
        self
    }

    pub fn is_not_nil(&self) -> bool {
        let env = self.env;
        unsafe_raw_call_no_exit!(env, is_not_nil, self.raw)
    }

    // TODO: Decide what we want for PartialEq (==, !=): eq vs. eql vs. equal.
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, other: Value<'e>) -> bool {
        let env = self.env;
        // Safety: `other` has the same lifetime.
        unsafe_raw_call_no_exit!(env, eq, self.raw, other.raw)
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
        self.get_raw_pointer().map(|r| &mut *r)
    }

    pub fn car<T: FromLisp<'e>>(self) -> Result<T> {
        self.env.call(subr::car, (self,))?.into_rust()
    }

    pub fn cdr<T: FromLisp<'e>>(self) -> Result<T> {
        self.env.call(subr::cdr, (self,))?.into_rust()
    }
}
