extern crate libc;
#[macro_use]
extern crate emacs;
extern crate libloading as lib;
#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::sync::Mutex;

use emacs::{Env, CallEnv, Value, Result, ResultExt};
use emacs::raw::emacs_env;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

lazy_static! {
    static ref LIBRARIES: Mutex<HashMap<String, lib::Library>> = Mutex::new(HashMap::new());
}

const INIT_FROM_ENV: &str = "emacs_rs_module_init";
const RS_MODULE: &str = "rs-module";

macro_rules! message {
    ($env:expr, $fmt:expr $(, $args:expr)*) => {
        $env.message(&format!($fmt $(, $args)*))
    };
}

macro_rules! ctx {
    ($call:expr) => {
        $call.context(RS_MODULE)
    }
}

/// Helper function that enables live-reloading of Emacs's dynamic module. To be reloadable, the
/// module be loaded by this function (`rs-module/load` in ELisp) instead of Emacs'
/// `module-load`. (Re)loading is achieved by calling `(rs-module/load "/path/to/module")`.
fn load_module(env: &CallEnv) -> Result<Value> {
    let path: String = env.parse_arg(0)?;
    let mut libraries = LIBRARIES.lock()
        .expect("Failed to acquire lock for module map");
    // TODO: How about tracking by feature name?
    match libraries.remove(&path) {
        Some(l) => message!(env, "[{}]: unloaded {:?}...", &path, &l)?,
        None => message!(env, "[{}]: not loaded yet", &path)?,
    };
    message!(env, "[{}]: loading...", &path)?;
    let l = ctx!(lib::Library::new(&path))?;
    message!(env, "[{}]: initializing...", &path)?;
    unsafe {
        let rs_init: lib::Symbol<unsafe extern fn(*mut emacs_env) -> u32> =
            ctx!(l.get(INIT_FROM_ENV.as_bytes()))?;
        rs_init(env.raw());
    }
    libraries.insert(path.clone(), l);
    message!(env, "[{}]: loaded and initialized", &path)
}

/// This is not exported, since this module should be loaded by Emacs's built-in `module-load`, thus
/// cannot be reloaded.
fn init(env: &Env) -> Result<Value> {
    message!(env, "[{}]: defining functions...", RS_MODULE)?;
    emacs_export_functions! {
        env, format!("{}/", RS_MODULE), {
            "load" => (load_module, 1..1, format!("Load a dynamic module that defines {}.", INIT_FROM_ENV)),
        },
    }
    env.provide(RS_MODULE)
}
