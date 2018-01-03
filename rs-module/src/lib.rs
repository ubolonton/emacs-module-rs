extern crate libc;
#[macro_use]
extern crate emacs;
extern crate libloading as lib;
#[macro_use]
extern crate lazy_static;

use emacs::{EmacsVal, EmacsRT, EmacsEnv};
use emacs::{Env, Result, HandleFunc};
use std::ptr;
use std::collections::HashMap;
use std::sync::Mutex;

/// This states that the module is GPL-compliant.
/// Emacs won't load the module if this symbol is undefined.
#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

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

/// Helper function that enables live-reloading of Emacs's dynamic module. To be reloadable, the
/// module be loaded by this function (`rs-module/load` in ELisp) instead of Emacs'
/// `module-load`. (Re)loading is achieved by calling `(rs-module/load "/path/to/module")`.
fn load_module(env: &mut Env, args: &[EmacsVal], _data: *mut libc::c_void) -> Result<EmacsVal> {
    let path: String = env.from_emacs(args[0])?;
    let mut libraries = LIBRARIES.lock()
        .expect("Failed to acquire lock for module map");
    // TODO: How about tracking by feature name?
    match libraries.remove(&path) {
        Some(l) => message!(env, "[{}]: unloaded {:?}...", &path, &l)?,
        None => message!(env, "[{}]: not loaded yet", &path)?,
    };
    message!(env, "[{}]: loading...", &path)?;
    let l = lib::Library::new(&path)?;
    message!(env, "[{}]: initializing...", &path)?;
    unsafe {
        let rs_init: lib::Symbol<unsafe extern fn(*mut EmacsEnv) -> u32> =
            l.get(INIT_FROM_ENV.as_bytes())?;
        rs_init(env.raw());
    }
    libraries.insert(path.clone(), l);
    message!(env, "[{}]: loaded and initialized", &path)
}

/// This is not exported, since this module should be loaded by Emacs's built-in `module-load`, thus
/// cannot be reloaded.
fn init(env: &mut Env) -> Result<EmacsVal> {
    message!(env, "[{}]: loading...", RS_MODULE)?;
    emacs_subrs!(
        load_module -> f_load_module;
    );
    message!(env, "[{}]: defining functions...", RS_MODULE)?;
    env.register(
        &format!("{}/load", RS_MODULE),f_load_module, 1..1,
        &format!("Load a dynamic module that defines {}.", INIT_FROM_ENV),
        ptr::null_mut()
    )?;
    env.provide(RS_MODULE)
}

#[no_mangle]
pub extern "C" fn emacs_module_init(ert: *mut EmacsRT) -> libc::c_int {
    match init(&mut Env::from(ert)) {
        Ok(_) => 0,
        Err(_) => 0,
    }
}
