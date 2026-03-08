use emacs::{defun, Env, Result, Value};

/// Open a channel to PROCESS, returning the file descriptor.
#[cfg(unix)]
#[defun]
fn open_channel(env: &Env, process: Value<'_>) -> Result<i32> {
    env.open_channel(process)
}

/// Open a channel to PROCESS, write DATA to it, then close the fd.
#[cfg(unix)]
#[defun]
fn channel_send(env: &Env, process: Value<'_>, data: String) -> Result<()> {
    use std::io::Write;
    use std::os::fd::FromRawFd;

    let fd = env.open_channel(process)?;
    let mut file = unsafe { std::fs::File::from_raw_fd(fd) };
    file.write_all(data.as_bytes())?;
    Ok(())
}

/// Open a channel to PROCESS, then spawn a thread that writes DATA and closes the fd.
/// Blocks until the thread finishes.
#[cfg(unix)]
#[defun]
fn channel_send_from_thread(env: &Env, process: Value<'_>, data: String) -> Result<()> {
    use std::io::Write;
    use std::os::fd::FromRawFd;

    let fd = env.open_channel(process)?;
    let handle = std::thread::spawn(move || -> std::io::Result<()> {
        let mut file = unsafe { std::fs::File::from_raw_fd(fd) };
        file.write_all(data.as_bytes())?;
        Ok(())
    });
    handle.join().expect("thread panicked")?;
    Ok(())
}
