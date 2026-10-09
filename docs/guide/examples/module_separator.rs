use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

// ANCHOR: example
    // Use `/` as the separator that goes after feature name, like some other packages.
    #[emacs::module(separator = "/")]
    fn init(_: &Env) -> Result<()> { Ok(()) }
// ANCHOR_END: example
