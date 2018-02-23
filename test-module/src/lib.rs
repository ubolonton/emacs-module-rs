extern crate libc;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate emacs;

use emacs::{Env, Value, Result};

#[macro_use]
mod macros;

mod test_basics;
mod test_error;
mod test_transfer;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

const MODULE: &str = "t";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

fn init(env: &Env) -> Result<Value> {
    env.message("Hello, Emacs!")?;

    test_basics::init(env)?;
    test_error::init(env)?;
    test_transfer::init(env)?;

    env.provide(MODULE)
}
