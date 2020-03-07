#![allow(non_upper_case_globals)]

use crate::{Result, Env, types::OnceGlobalRef, init::__INIT_FNS__};
use ctor::ctor;

global_refs! {
    cons car cdr
    message
    vector
    make_vector
}

fn initialize(env: &Env) -> Result<()> {
    macro_rules! init {
        ($($name:ident => $sym:literal,)*) => {
            $(OnceGlobalRef::init_to_function(&$name, env, $sym)?;)*
        };
        ($($name:ident)*) => {
            $(OnceGlobalRef::init_to_function(&$name, env, stringify!($name))?;)*
        };
    }
    init! {
        cons car cdr
        vector
        message
    }
    init! {
        make_vector => "make-vector",
    }
    Ok(())
}

#[ctor]
fn register_initializer() {
    __INIT_FNS__.lock().expect("Failed to acquire a write lock on map of initializers")
        // XXX: In theory, this can conflict with a user-defined function name.
        .insert("__emacs_module_rs_initialize_subrs__".into(), Box::new(initialize));
}
