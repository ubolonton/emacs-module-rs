//! Initialization machinery. It should be mainly used by the #[[`module`]] macro, not module code.
//!
//! [`module`]: attr.module.html

use std::{
    os, panic,
    collections::HashMap,
    sync::{Mutex, atomic::AtomicBool},
};

use once_cell::sync::Lazy;

use crate::{Env, Value, Result};

#[doc(hidden)]
#[macro_export]
macro_rules! __module_init {
    ($init:ident) => {
        /// Entry point for Emacs's module loader.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_module_init(
            runtime: *mut $crate::raw::emacs_runtime,
        ) -> ::std::os::raw::c_int {
            $crate::init::initialize(&$crate::Env::from_runtime(runtime), $init)
        }

        // TODO: Exclude this in release build.
        /// Entry point for live-reloading (by `rs-module`) during development.
        #[no_mangle]
        pub unsafe extern "C" fn emacs_rs_module_init(
            raw: *mut $crate::raw::emacs_env,
        ) -> ::std::os::raw::c_int {
            $crate::init::initialize(&$crate::Env::new(raw), $init)
        }
    };
}

type InitFn = Box<dyn Fn(&Env) -> Result<()> + Send + 'static>;

type FnMap = HashMap<String, InitFn>;

// NOTE: Keep the lazy statics' names in-sync with those declared in emacs_macros::util.
// TODO: How about defining these in user crate, and requiring #[module] to be at the crate's root?
// TODO: We probably don't need the mutexes.

/// Functions to be called first, when the dynamic library is loaded by the OS, before Emacs calls
/// `emacs_module_init`. These are only called if the attribute macro #[[`module`]] is used, instead
/// of the [`module_init!`] macro.
///
/// [`module_init!`]: macro.module_init.html
/// [`module`]: attr.module.html
pub static __PRE_INIT__: Lazy<Mutex<Vec<InitFn>>> = Lazy::new(|| Mutex::new(vec![]));

// Re-explain this. These functions are for exposing #[defun] functions.
/// Functions to be called when the dynamic module is loaded by the OS, before Emacs calls
/// `emacs_module_init`. These are only called if #[[`module`]] attribute macro is used,
/// instead of [`module_init!`] macro.
///
/// [`module_init!`]: macro.module_init.html
/// [`module`]: attr.module.html
pub static __INIT_FNS__: Lazy<Mutex<FnMap>> = Lazy::new(|| Mutex::new(HashMap::new()));

/// Prefix to prepend to name of every Lisp function exposed by the dynamic module through the
/// attribute macro #[[`defun`]].
///
/// [`defun`]: attr.defun.html
pub static __PREFIX__: Lazy<Mutex<[String; 2]>> = Lazy::new(|| Mutex::new(["".to_owned(), "-".to_owned()]));

pub static __MOD_IN_NAME__: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(true));

fn debugging() -> bool {
    std::env::var("EMACS_MODULE_RS_DEBUG").unwrap_or_default() == "1"
}

fn check_gc_bug_31238(env: &Env) -> Result<()> {
    let version = env.call("default-value", [env.intern("emacs-version")?])?;
    let fixed = env.call("version<=", ("27", version))?.is_not_nil();
    if debugging() {
        env.call("set", (
            env.intern("module-rs-disable-gc-bug-31238-workaround")?,
            // Can't use true/false directly because symbol mod's globals have not been initialized.
            env.intern(match fixed {
                true => "t",
                false => "nil",
            })?
        ))?;
    }
    crate::env::HAS_FIXED_GC_BUG_31238.get_or_init(|| fixed);
    Ok(())
}

#[inline]
pub fn initialize<F>(env: &Env, f: F) -> os::raw::c_int
where
    F: Fn(&Env) -> Result<Value<'_>> + panic::RefUnwindSafe,
{
    let env = panic::AssertUnwindSafe(env);
    let result = panic::catch_unwind(|| match env.define_errors()
        .and_then(|_| check_gc_bug_31238(&env))
        .and_then(|_| f(&env)) {
        Ok(_) => 0,
        Err(e) => {
            env.message(format!("Error during initialization: {:#?}", e))
                .expect("Fail to message Emacs about error");
            1
        }
    });
    match result {
        Ok(v) => v,
        Err(e) => {
            env.message(format!("Panic during initialization: {:#?}", e))
                .expect("Fail to message Emacs about panic");
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
