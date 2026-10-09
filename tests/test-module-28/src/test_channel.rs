use std::io::Write;

use emacs::{defun, Env, Result, Value};

/// Open a channel to PROCESS, write DATA to it, then close.
#[defun]
fn channel_send(env: &Env, process: Value<'_>, data: String) -> Result<()> {
    let mut writer = env.open_channel(process)?;
    writer.write_all(data.as_bytes())?;
    Ok(())
}

/// Open a channel to PROCESS, then spawn a thread that writes DATA and closes it.
/// Blocks until the thread finishes.
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
