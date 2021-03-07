use std::ops::Deref;

use once_cell::sync::OnceCell;

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
/// The above is a shortcoming in the design of emacs-module. There are 2 possible ways to fix it:
/// - Make [`free_global_ref`] work without an env, like Erlang's `enif_release_resource`.
/// - Allow `user_ptr`'s finalizer to access the env, to properly free associated global refs.
///
/// [`Env`]: struct.Env.html
/// [`Value`]: struct.Value.html
/// [`defun`]: attr.defun.html
/// [`Clone`]: https://doc.rust-lang.org/std/clone/trait.Clone.html
/// [`free_global_ref`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Values.html
/// [`free`]: #method.free
/// [`drop`]: https://doc.rust-lang.org/std/mem/fn.drop.html
#[derive(Debug)]
#[repr(transparent)]
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
        // NOTE: raw != value.raw
        Self { raw }
    }

    // For testing.
    pub(crate) unsafe fn from_raw(raw: emacs_value) -> Self {
        Self { raw }
    }

    /// Frees this global reference.
    pub fn free(self, env: &Env) -> Result<()> {
        // Safety: We assume user code doesn't directly call C function `free_global_ref`.
        unsafe_raw_call!(env, free_global_ref, self.raw)?;
        Ok(())
    }

    /// Returns the underlying [`Value`], scoping its lifetime to the given [`Env`].
    ///
    /// [`Env`]: struct.Env.html
    /// [`Value`]: struct.Value.html
    #[inline]
    pub fn bind<'e, 'g: 'e>(&'g self, env: &'e Env) -> Value<'e> {
        // Safety: This global ref keeps the underlying Lisp object alive.
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
        Ok(self.bind(env))
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

/// Declares global references. These will be initialized when the module is loaded.
#[doc(hidden)]
#[macro_export]
macro_rules! global_refs {
    ($($name:ident)*) => {
        $(
            #[allow(non_upper_case_globals)]
            pub static $name: &'static $crate::OnceGlobalRef = {
                static X: $crate::OnceGlobalRef = $crate::OnceGlobalRef::new();
                &X
            };
        )*
    };
    ($registrator_name:ident ($init_method:ident) =>
        $(
            $name:ident $( => $lisp_name:expr )?
        )*
    ) => {
        $crate::global_refs! {
            $($name)*
        }

        #[$crate::deps::ctor::ctor]
        fn $registrator_name() {
            $crate::init::__PRE_INIT__.try_lock()
                .expect("Failed to acquire a write lock on the list of initializers")
                .push(::std::boxed::Box::new(|env| {
                    $(
                        #[allow(unused_variables)]
                        let name = $crate::deps::emacs_macros::lisp_name!($name);
                        $( let name = $lisp_name; )?
                        $crate::OnceGlobalRef::$init_method(&$name, env, name)?;
                    )*
                    Ok(())
                }));
        }
    };
}

/// A [`GlobalRef`] that can be initialized once. This is useful for long-lived values that should
/// be initialized when the dynamic module is loaded. A typical use case is specifying
/// frequently-used symbols, which can be done with the help of the macro [`use_symbols!`].
///
/// [`use_symbols`]: crate::use_symbols
#[derive(Debug)]
#[repr(transparent)]
pub struct OnceGlobalRef {
    inner: OnceCell<GlobalRef>
}

impl OnceGlobalRef {
    pub const fn new() -> Self {
        Self { inner: OnceCell::new() }
    }

    /// Initializes this global reference with the given function.
    #[doc(hidden)]
    pub fn init<F: FnOnce(&Env) -> Result<Value>>(&self, env: &Env, f: F) -> Result<&GlobalRef> {
        let g = f(env)?.make_global_ref();
        self.inner.set(g).expect("Cannot initialize a global reference more than once");
        Ok(self.inner.get().expect("Failed to get an initialized OnceGlobalRef"))
    }

    /// Points this global reference to an interned Lisp symbol with the given name.
    ///
    /// This should be called once, during module initialization.
    #[doc(hidden)]
    pub fn init_to_symbol(&self, env: &Env, name: &str) -> Result<&GlobalRef> {
        self.init(env, |env| env.intern(name))
    }

    /// Points this global reference to the function bound to the Lisp symbol with the given name.
    ///
    /// This should be called once, during module initialization.
    ///
    /// If the symbol is later bound to another function, this global reference will still point to
    /// the old function. Therefore, this is best used for built-in and primitive functions.
    #[doc(hidden)]
    pub fn init_to_function(&self, env: &Env, name: &str) -> Result<&GlobalRef> {
        self.init(env, |env| {
            let symbol = env.intern(name)?;
            env.call("indirect-function", [symbol])
        })
    }
}

impl<'e> IntoLisp<'e> for &'e OnceGlobalRef {
    #[inline(always)]
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        Ok(self.bind(env))
    }
}

impl Deref for OnceGlobalRef {
    type Target = GlobalRef;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner.get().expect("Cannot access an uninitialized global reference")
    }
}
