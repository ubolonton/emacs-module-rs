// ANCHOR: fill
use emacs::{defun, Env, Result, Value};

/// Fill CANVAS with PIXEL.
#[defun]
fn fill(env: &Env, canvas: Value<'_>, pixel: u32) -> Result<()> {
    canvas.with_canvas_data(|data| data.buffer.fill(pixel))?;
    env.call("canvas-refresh", [canvas])?;
    Ok(())
}
// ANCHOR_END: fill

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
