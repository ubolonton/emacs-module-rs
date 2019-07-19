use std::{
    collections::HashMap,
    sync::Mutex,
};

use emacs::{defun, Env, Value, Result};
use emacs::raw::emacs_env;

use libloading::{Library, Symbol};

emacs::plugin_is_GPL_compatible!();

emacs::deps::lazy_static::lazy_static! {
    static ref LIBRARIES: Mutex<HashMap<String, Library>> = Mutex::new(HashMap::new());
}

const INIT_FROM_ENV: &str = "emacs_rs_module_init";

macro_rules! message {
    ($env:expr, $fmt:expr $(, $args:expr)*) => {
        $env.message(format!($fmt $(, $args)*))
    };
}

// This module should be loaded by Emacs's built-in `module-load`, so it cannot be reloaded.
#[emacs::module(name = "rs-module", separator = "/")]
fn init(env: &Env) -> Result<Value<'_>> {
    message!(env, "[rs-module]: defined functions...")
}

/// Helper function that enables live-reloading of Emacs's dynamic module. To be reloadable, the
/// module be loaded by this function (`rs-module/load` in ELisp) instead of Emacs'
/// `module-load`. (Re)loading is achieved by calling `(rs-module/load "/path/to/module")`.
#[defun]
fn load(env: &Env, path: String) -> Result<Value<'_>> {
    let mut libraries = LIBRARIES.lock()
        .expect("Failed to acquire lock for module map");
    // TODO: How about tracking by feature name?
    match libraries.remove(&path) {
        Some(l) => message!(env, "[{}]: unloaded {:?}...", path, l)?,
        None => message!(env, "[{}]: not loaded yet", path)?,
    };
    message!(env, "[{}]: loading...", path)?;
    let lib = Library::new(&path)?;
    message!(env, "[{}]: initializing...", path)?;
    unsafe {
        let rs_init: Symbol<'_, unsafe extern fn(*mut emacs_env) -> u32> =
            lib.get(INIT_FROM_ENV.as_bytes())?;
        rs_init(env.raw());
    }
    libraries.insert(path.clone(), lib);
    message!(env, "[{}]: loaded and initialized", path)
}
