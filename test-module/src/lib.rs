use lazy_static::lazy_static;

use emacs;
use emacs::{Env, Value, Result};

#[macro_use]
mod macros;

mod test_basics;
mod test_error;
mod test_transfer;
mod test_lifetime;

emacs::emacs_plugin_is_GPL_compatible!();

const MODULE: &str = "t";
lazy_static! {
    static ref MODULE_PREFIX: String = format!("{}/", MODULE);
}

#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Hello, Emacs!")?;

    test_basics::init(env)?;
    test_error::init(env)?;
    test_transfer::init(env)?;
    test_lifetime::init(env)?;

    env.provide(MODULE)
}
