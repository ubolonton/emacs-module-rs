use emacs::{Env, Result};

// ANCHOR: init
emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(env: &Env) -> Result<()> {
    // This is run when Emacs loads the module.
    // More concretely, it is run after all the functions it defines are exported,
    // but before `(provide 'feature-name)` is (automatically) called.
    Ok(())
}
// ANCHOR_END: init
