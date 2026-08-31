//! Initialization machinery. It should be mainly used by the #[[`module`]] macro, not module code.
//!
//! [`module`]: attr.module.html

use std::{
    os, panic,
    collections::HashMap,
    sync::{LazyLock, Mutex, atomic::AtomicBool},
};

use anyhow::anyhow;
use emacs_module::{EMACS_VERSION, MIN_ENV_SIZE};

use crate::{Env, Value, Result, ErrorKind};

#[doc(hidden)]
#[macro_export]
macro_rules! __module_init {
    ($init:ident) => {
        /// Entry point for Emacs's module loader.
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn emacs_module_init(
            runtime: *mut $crate::raw::emacs_runtime,
        ) -> ::std::os::raw::c_int {
            $crate::init::initialize(&$crate::Env::from_runtime(runtime), $init)
        }

        // TODO: Exclude this in release build.
        /// Entry point for live-reloading (by `rs-module`) during development.
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn emacs_rs_module_init(
            raw: *mut $crate::raw::emacs_env,
        ) -> ::std::os::raw::c_int {
            $crate::init::initialize(&$crate::Env::new(raw), $init)
        }
    };
}

type InitFn = Box<dyn Fn(&Env) -> Result<()> + Send + 'static>;

type FnMap = HashMap<String, InitFn>;

// TODO: How about defining these in user crate, and requiring #[module] to be at the crate's root?
// TODO: We probably don't need the mutexes.

/// Functions that will be called by [`emacs_module_init`] to initialize global references to
/// frequently used Lisp values.
///
/// They are called before loading module metadata, e.g. module name, function prefix.
///
/// This list is populated when the OS loads the dynamic library, before Emacs calls
/// [`emacs_module_init`].
///
/// [`emacs_module_init`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html
pub static __GLOBAL_REFS__: LazyLock<Mutex<Vec<InitFn>>> = LazyLock::new(|| Mutex::new(vec![]));

/// Functions that will be called by [`emacs_module_init`] to define custom error signals.
///
/// They are called before loading module metadata, e.g. module name, function prefix.
///
/// This list is populated when the OS loads the dynamic library, before Emacs calls
/// [`emacs_module_init`].
///
/// [`emacs_module_init`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html
pub static __CUSTOM_ERRORS__: LazyLock<Mutex<Vec<InitFn>>> = LazyLock::new(|| Mutex::new(vec![]));

/// Functions that will be called by [`emacs_module_init`] to define the module functions.
///
/// They are called after loading module metadata, e.g. module name, function prefix.
///
/// This map is populated when the OS loads the dynamic library, before Emacs calls
/// [`emacs_module_init`].
///
/// [`emacs_module_init`]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html
pub static __INIT_FNS__: LazyLock<Mutex<FnMap>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// Prefix to prepend to name of every Lisp function exposed by the dynamic module through the
/// attribute macro #[[`defun`]].
///
/// [`defun`]: attr.defun.html
pub static __PREFIX__: LazyLock<Mutex<[String; 2]>> =
    LazyLock::new(|| Mutex::new(["".to_owned(), "-".to_owned()]));

pub static __MOD_IN_NAME__: LazyLock<AtomicBool> = LazyLock::new(|| AtomicBool::new(true));

fn debugging() -> bool {
    std::env::var("EMACS_MODULE_RS_DEBUG").unwrap_or_default() == "1"
}

/// Detects loading into an Emacs older than what an enabled `emacs-N` feature requires, e.g.
/// calling `Env::open_channel` (which needs `emacs-28`) on a struct that Emacs 27 allocated would
/// read past its end.
fn check_abi_compatible(env: &Env) -> Result<()> {
    // Safety: `size` is the struct's first field, at the same offset in every version since Emacs
    // 25 (the oldest ABI this crate supports), so reading it doesn't depend on which version of
    // `emacs_env` the running Emacs actually allocated.
    let actual = unsafe { (*env.raw()).size } as usize;
    if actual < MIN_ENV_SIZE {
        return Err(anyhow!(
            "This module requires Emacs {EMACS_VERSION}+, but the running Emacs's module ABI is \
             older: env struct is {actual} bytes, at least {MIN_ENV_SIZE} are required"
        ));
    }
    Ok(())
}

fn check_gc_bug_31238(env: &Env) -> Result<()> {
    let version = env.call("default-value", [env.intern("emacs-version")?])?;
    let fixed = env.call("version<=", ("27", version))?.is_not_nil();
    if debugging() {
        env.call("set", (env.intern("module-rs-disable-gc-bug-31238-workaround")?, fixed))?;
    }
    crate::env::HAS_FIXED_GC_BUG_31238.get_or_init(|| fixed);
    Ok(())
}

#[inline]
pub fn initialize<F>(env: &Env, init: F) -> os::raw::c_int
where
    F: Fn(&Env) -> Result<Value<'_>> + panic::RefUnwindSafe,
{
    let env = panic::AssertUnwindSafe(env);
    let result = panic::catch_unwind(|| {
        match (|| {
            for init_global_ref in __GLOBAL_REFS__
                .try_lock()
                .expect("Failed to acquire a read lock on the list of initializers for global-refs")
                .iter()
            {
                init_global_ref(&env)?;
            }
            // This must run after the loop above, which initializes `subr::message` (among
            // others): reporting a failure below goes through `Env::message`, which needs it.
            check_abi_compatible(&env)?;
            env.define_core_errors()?;
            check_gc_bug_31238(&env)?;
            for define_error in __CUSTOM_ERRORS__.try_lock()
            .expect("Failed to acquire a read lock on the list of initializers for custom error signals").iter() {
            define_error(&env)?;
        }
            init(&env)
        })() {
            Ok(_) => 0,
            Err(e) => {
                if let Some(ErrorKind::Signal { symbol, data }) = e.downcast_ref::<ErrorKind>() {
                    env.call(
                        "message",
                        (
                            "Error during initialization: symbol: %s data: %s",
                            unsafe { symbol.value(&env) },
                            unsafe { data.value(&env) },
                        ),
                    )
                } else {
                    env.message(format!("Error during initialization: {:#?}", e))
                }
                .expect("Failed to message Emacs about initialization error");
                1
            }
        }
    });
    match result {
        Ok(v) => v,
        Err(e) => {
            env.message(format!("Panic during initialization: {:#?}", e))
                .expect("Failed to message Emacs about initialization panic");
            2
        }
    }
}

fn lisp_name(s: &str) -> String {
    s.replace("_", "-")
}

pub fn lisp_pkg(mod_path: &str) -> String {
    let crate_name = mod_path.split("::").nth(0).expect("mod_path is empty!");
    lisp_name(&crate_name)
}

pub fn lisp_path(mod_path: &str) -> String {
    let split = mod_path.split("::");
    let mut path =
        __PREFIX__.try_lock().expect("Failed to acquire read lock of module prefix").join("");
    for segment in split.skip(1) {
        path.push_str(segment);
        path.push('-');
    }
    lisp_name(&path)
}
