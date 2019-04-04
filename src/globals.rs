use std::{
    collections::HashMap,
    sync::Mutex,
};

use lazy_static::lazy_static;

use crate::{Env, Result};

type InitFn = Fn(&Env) -> Result<()> + Send + 'static;

type FnMap = HashMap<String, Box<InitFn>>;

// TODO: How about defining these in user crate, and requiring #[module] to be at the crate's root?
lazy_static! {
    /// Functions to be called when Emacs loads the dynamic module. These are only called if
    /// [`#[module]`] attribute macro is used, instead of [emacs_module_init!] macro.
    ///
    /// [emacs_module_init!]: macro.emacs_module_init.html
    /// [`#[module]`]: ../emacs_macros/attr.module.html
    pub static ref __INIT_FNS__: Mutex<FnMap> = Mutex::new(HashMap::new());

    /// Prefix to prepend to name of every Lisp function exposed by the dynamic module through the
    /// attribute macro [`#[func]`].
    ///
    /// [`#[func]`]: ../emacs_macros/attr.func.html
    pub static ref __PREFIX__: Mutex<&'static str> = Mutex::new("");
}
