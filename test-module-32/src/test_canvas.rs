use emacs::{defun, Env, Result, Value};

/// Fill CANVAS with PIXEL. Return (WIDTH HEIGHT LENGTH) of the buffer.
#[defun]
fn canvas_fill<'e>(env: &'e Env, canvas: Value<'e>, pixel: u32) -> Result<Value<'e>> {
    let (width, height, length) = canvas.with_canvas_data(|data| {
        data.buffer.fill(pixel);
        (data.width, data.height, data.buffer.len())
    })?;
    env.list((width, height, length))
}

/// Return the pixel at INDEX of CANVAS, or nil if INDEX is out of range.
#[defun]
fn canvas_pixel(canvas: Value<'_>, index: usize) -> Result<Option<u32>> {
    canvas.with_canvas_data(|data| data.buffer.get(index).copied())
}
