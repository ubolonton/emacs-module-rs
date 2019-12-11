use emacs_module::emacs_value;

use super::*;

/// A "global reference" that can live outside the scope of an [`Env`]. This is useful for sharing
/// an otherwise short-lived Lisp [`Value`] across multiple invocations of Rust functions defined
/// with [`defun`]. Examples include efficient access to interned symbols or Lisp functions, and
/// Rust-based multi-threading.
///
/// # Implementation
///
/// Cloning this struct requires an [`Env`], so it doesn't implement [`Clone`].
///
/// [`free_global_ref`] requires an [`Env`]. Therefore, to avoid leaking the underlying [`Value`],
/// [`free`] should be used to free a global reference, instead of [`drop`]. For the use case of
/// accessing interned symbols and Lisp functions, this is a non-issue, as the values are
/// supposed to be "static" anyway.
///
/// [`Env`]: struct.Env.html
/// [`Value`]: struct.Value.html
/// [`defun`]: attr.defun.html
/// [`Clone`]: https://doc.rust-lang.org/std/clone/trait.Clone.html
/// [`free_global_ref`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Values.html
/// [`free`]: #method.free
/// [`drop`]: https://doc.rust-lang.org/std/mem/fn.drop.html
#[derive(Debug)]
pub struct GlobalRef {
    raw: emacs_value
}

impl GlobalRef {
    /// Creates a new global reference for the given [`Value`].
    ///
    /// [`Value`]: struct.Value.html
    pub fn new(value: Value) -> Self {
        let env = value.env;
        // TODO: Check whether this really is `no_exit`.
        let raw = unsafe_raw_call_no_exit!(env, make_global_ref, value.raw);
        // TODO: Check whether raw == self.raw
        Self { raw }
    }

    /// Frees this global reference.
    pub fn free(self, env: &Env) -> Result<()> {
        unsafe_raw_call!(env, free_global_ref, self.raw)?;
        Ok(())
    }

    /// Returns the underlying [`Value`], scoping its lifetime to the given [`Env`].
    ///
    /// [`Env`]: struct.Env.html
    /// [`Value`]: struct.Value.html
    #[inline]
    pub fn within<'e>(&'e self, env: &'e Env) -> Value<'e> {
        unsafe { Value::new(self.raw, env) }
    }

    /// Returns a copy of this global reference.
    pub fn clone(&self, env: &Env) -> Self {
        self.bind(env).make_global_ref()
    }
}

// Safety: Doing anything useful with a GlobalRef requires an &Env, which means holding the GIL.
unsafe impl Send for GlobalRef {}
unsafe impl Sync for GlobalRef {}

impl<'e> FromLisp<'e> for GlobalRef {
    #[inline(always)]
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        Ok(Self::new(value))
    }
}

impl<'e> IntoLisp<'e> for &'e GlobalRef {
    #[inline(always)]
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        Ok(self.within(env))
    }
}

impl<'e> Value<'e> {
    /// Creates a new [`GlobalRef`] for this value.
    ///
    /// [`GlobalRef`]: struct.GlobalRef.html
    #[inline(always)]
    pub fn make_global_ref(self) -> GlobalRef {
        GlobalRef::new(self)
    }
}
