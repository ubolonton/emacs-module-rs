#![allow(non_upper_case_globals)]

use crate::{Result, Env, types::OnceGlobalRef, init::__INIT_FNS__};
use ctor::ctor;

macro_rules! decl {
    ($($name:ident)*) => {
        $(pub static $name: OnceGlobalRef = OnceGlobalRef::new();)*
    }
}

decl! {
    nil t
}

fn initialize(env: &Env) -> Result<()> {
    macro_rules! init {
        ($($name:ident)*) => {
            $(OnceGlobalRef::init_to_symbol(&$name, env, stringify!($name))?;)*
        };
    }
    init! {
        nil t
    }
    Ok(())
}

#[ctor]
fn register_initializer() {
    __INIT_FNS__.lock().expect("Failed to acquire a write lock on map of initializers")
        // XXX: In theory, this can conflict with a user-defined function name.
        .insert("__emacs_module_rs_initialize_symbols_ ".into(), Box::new(initialize));
}
