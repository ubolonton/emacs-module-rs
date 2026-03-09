use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

mod test_channel;

#[emacs::module(name(fn), separator = "/", mod_in_name = false)]
fn t28(_env: &Env) -> Result<()> {
    Ok(())
}
