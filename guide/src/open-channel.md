# Communicating with Emacs from Background Threads

`open_channel` lets a background Rust thread send data to Emacs by writing into a pipe process's input stream. It requires the `emacs-28` feature and Emacs 28+.

## Setup

Enable the feature in `Cargo.toml`:

```toml
[dependencies.emacs]
version = "0.21"
features = ["emacs-28"]
```

## How It Works

Emacs's `make-pipe-process` returns a process object backed by a pair of OS pipes. `env.open_channel(pipe_process)` opens the write end of that pipe and returns an `impl Write + Send + Sync`. You can move it to any thread and write to it; Emacs's process filter function receives the data on the main thread.

```rust
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
```

On the Emacs side, create the pipe process and attach a filter:

```emacs-lisp
(let ((proc (make-pipe-process
             :name "my-pipe"
             :filter (lambda (_proc data)
                       (message "Received: %s" data)))))
  (my-module-channel-send proc "hello from Rust"))
```

Dropping the writer closes the write end of the pipe. Emacs's process sentinel fires when the last writer closes.

## Windows Note

On Windows, the write end of the pipe is a CRT (C runtime) file descriptor created by Emacs. Your module must link against the same CRT as Emacs. Otherwise, `open_channel` crashes inside `get_osfhandle`.

- Official GNU builds use MSVCRT. Build your module in the MSYS2 MINGW64 shell.
- Official `-UCRT64` builds (Emacs 31+) and MSYS2's `mingw-w64-ucrt-x86_64-emacs` use UCRT. Build your module in the MSYS2 UCRT64 shell.

The GNU toolchain uses `gcc` as the linker, so the first `gcc` in `PATH` selects the CRT. To check a binary's CRT, list its imported DLLs:

```bash
objdump -p "$(which emacs)" | grep 'DLL Name'
```

`msvcrt.dll` means MSVCRT. `ucrtbase.dll` or `api-ms-win-crt-*.dll` means UCRT. See [Overview](./overview.md#setting-up) for the setup steps.
