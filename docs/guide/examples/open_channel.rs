// ANCHOR: example
use std::io::Write;
use emacs::{defun, Env, Result, Value};

/// Send DATA to PROCESS from the calling thread.
#[defun]
fn channel_send(env: &Env, process: Value<'_>, data: String) -> Result<()> {
    let mut writer = env.open_channel(process)?;
    writer.write_all(data.as_bytes())?;
    Ok(())
}

/// Spawn a thread that sends DATA to PROCESS, then wait for it.
#[defun]
fn channel_send_from_thread(env: &Env, process: Value<'_>, data: String) -> Result<()> {
    let mut writer = env.open_channel(process)?;
    let handle = std::thread::spawn(move || -> std::io::Result<()> {
        writer.write_all(data.as_bytes())?;
        Ok(())
    });
    handle.join().expect("thread panicked")?;
    Ok(())
}
// ANCHOR_END: example

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
