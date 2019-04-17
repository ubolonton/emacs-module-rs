use std::{collections::HashMap, sync::{Mutex, atomic::AtomicBool}};

use lazy_static::lazy_static;

use crate::{Env, Result};

type InitFn = Fn(&Env) -> Result<()> + Send + 'static;

type FnMap = HashMap<String, Box<InitFn>>;

// TODO: How about defining these in user crate, and requiring #[module] to be at the crate's root?
// TODO: We probably don't need the mutexes.
lazy_static! {
    // Keep these names in-sync with those declared in emacs_macros::util.

    /// Functions to be called when Emacs loads the dynamic module. These are only called if
    /// [`#[module]`] attribute macro is used, instead of [module_init!] macro.
    ///
    /// [module_init!]: macro.module_init.html
    /// [`#[module]`]: ../emacs_macros/attr.module.html
    pub static ref __INIT_FNS__: Mutex<FnMap> = Mutex::new(HashMap::new());

    /// Prefix to prepend to name of every Lisp function exposed by the dynamic module through the
    /// attribute macro [`#[defun]`].
    ///
    /// [`#[defun]`]: ../emacs_macros/attr.defun.html
    pub static ref __PREFIX__: Mutex<[String; 2]> = Mutex::new(["".to_owned(), "-".to_owned()]);

    pub static ref __MOD_IN_NAME__: AtomicBool = AtomicBool::new(true);
}

fn lisp_name(s: &str) -> String {
    s.replace("_", "-")
}

pub fn lisp_pkg(module_path: &str) -> String {
    let crate_name = module_path.split("::").nth(0).expect("module_path is empty!");
    lisp_name(&crate_name)
}

pub fn lisp_path(module_path: &str) -> String {
    let split = module_path.split("::");
    let mut path =
        __PREFIX__.try_lock().expect("Failed to acquire read lock of module prefix").join("");
    for segment in split.skip(1) {
        path.push_str(segment);
        path.push('-');
    }
    lisp_name(&path)
}
