use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

mod test_canvas;

#[emacs::module(name(fn), separator = "/", mod_in_name = false)]
fn t32(_env: &Env) -> Result<()> {
    Ok(())
}
