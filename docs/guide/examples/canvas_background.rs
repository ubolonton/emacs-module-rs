// ANCHOR: example
use std::mem;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use emacs::{defun, Env, Result, Value};

struct Frame {
    width: usize,
    height: usize,
    pixels: Vec<u32>,
}

static FRAME: Mutex<Frame> = Mutex::new(Frame { width: 0, height: 0, pixels: Vec::new() });

/// Render frames of WIDTH by HEIGHT pixels in a background thread.
#[defun]
fn render_start(width: usize, height: usize) -> Result<()> {
    thread::spawn(move || {
        let mut back = Frame { width, height, pixels: Vec::new() };
        for tick in 0u32.. {
            back.width = width;
            back.height = height;
            back.pixels.resize(width * height, 0);
            for (index, pixel) in back.pixels.iter_mut().enumerate() {
                *pixel = 0xFF00_0000 | (index as u32).wrapping_add(tick.wrapping_mul(1000));
            }
            let Ok(mut front) = FRAME.lock() else { return };
            mem::swap(&mut *front, &mut back);
            drop(front);
            thread::sleep(Duration::from_millis(16));
        }
    });
    Ok(())
}

/// Copy the latest frame into CANVAS, and refresh it.
#[defun]
fn render_present(env: &Env, canvas: Value<'_>) -> Result<()> {
    let copied = canvas.with_canvas_data(|data| {
        let Ok(frame) = FRAME.lock() else { return false };
        if frame.width != data.width || frame.height != data.height {
            return false;
        }
        data.buffer.copy_from_slice(&frame.pixels);
        true
    })?;
    if copied {
        env.call("canvas-refresh", [canvas])?;
    }
    Ok(())
}
// ANCHOR_END: example

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
