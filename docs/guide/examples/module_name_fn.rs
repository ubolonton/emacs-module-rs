use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

// ANCHOR: example
    // Putting `rs` in crate's name is discouraged so we use the function's name
    // instead. The feature will be `rs-module-helper`.
    #[emacs::module(name(fn))]
    fn rs_module_helper(_: &Env) -> Result<()> { Ok(()) }
// ANCHOR_END: example
