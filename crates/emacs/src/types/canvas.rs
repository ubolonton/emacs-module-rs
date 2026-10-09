//! Access to the pixel buffer of canvas images (Emacs 32+).
//!
//! The Emacs behavior that the safety argument depends on is in `docs/research/canvas-data.md`.

use std::slice;

use crate::{subr, Result, Value};

crate::use_symbols! {
    data_width => ":data-width"
    data_height => ":data-height"
}

crate::use_functions! {
    plist_get
}

/// The pixel buffer of a canvas image, lent by [`Value::with_canvas_data`].
///
/// Experimental: tracks an unreleased module ABI.
#[non_exhaustive]
#[derive(Debug)]
pub struct CanvasData<'a> {
    /// Pixels in row-major order, `width * height` elements. Each pixel is a native-endian
    /// `0xAARRGGBB` value. Emacs ignores alpha when it displays the canvas.
    pub buffer: &'a mut [u32],
    /// Width, in pixels.
    pub width: usize,
    /// Height, in pixels.
    pub height: usize,
}

impl<'e> Value<'e> {
    /// Calls `f` with the pixel buffer of this canvas image spec, for example
    /// `(image :type canvas :id my-canvas :data-width 800 :data-height 600)`.
    ///
    /// Use this to draw into a canvas from Rust. Emacs does not show the changes until Lisp calls
    /// `canvas-refresh` on the spec. The [guide] shows how to draw from a background thread.
    ///
    /// Requires Emacs 32+, built with a window system. Experimental: tracks an unreleased module
    /// ABI.
    ///
    /// # Why a closure, and why `Send`
    ///
    /// The buffer is valid only while no Lisp code runs. Lisp code can resize or free the canvas,
    /// and redisplay reads the buffer. `f` must be `Send`, so it cannot capture `&Env` or `Value`,
    /// which are not `Send`. Thus `f` cannot run Lisp code. Because of this bound, `f` also cannot
    /// capture other values that are not `Send`, such as `Rc`. Copy the data out of them first.
    ///
    /// This compiles:
    ///
    /// ```
    /// use emacs::{defun, Env, Result, Value};
    ///
    /// #[defun]
    /// fn fill(env: &Env, canvas: Value<'_>, pixel: u32) -> Result<()> {
    ///     canvas.with_canvas_data(|data| data.buffer.fill(pixel))?;
    ///     env.call("canvas-refresh", [canvas])?;
    ///     Ok(())
    /// }
    /// ```
    ///
    /// This does not compile, because `f` captures `env`:
    ///
    /// ```compile_fail,E0277
    /// use emacs::{defun, Env, Result, Value};
    ///
    /// #[defun]
    /// fn fill(env: &Env, canvas: Value<'_>) -> Result<()> {
    ///     canvas.with_canvas_data(|data| {
    ///         let _ = env.message("drawing");
    ///         data.buffer.fill(0);
    ///     })
    /// }
    /// ```
    ///
    /// # Known gap
    ///
    /// Lisp code can run while this method reads the dimensions, for example `post-gc-hook` or
    /// the debugger. Lisp code can also run inside `canvas_data`: file name handlers for `:file`,
    /// and `*Messages*` updates (`messages-buffer-mode` and its hooks) when Emacs reports an image
    /// error. If that code changes the dimensions of the same canvas, `buffer` has the wrong
    /// length. Do not resize a canvas from such code.
    ///
    /// The length is also wrong if Lisp redefines `cdr` or `plist-get` to return false values
    /// before the module loads. The module calls the definitions that exist at load time. Advice
    /// that passes the real results through, such as `trace-function`, is harmless.
    ///
    /// # Errors
    ///
    /// | Rust variant | Lisp signal, if the error propagates |
    /// |---|---|
    /// | [`ModuleError::Signal`](crate::ModuleError::Signal), `(error "Not a canvas")` | `error` |
    /// | [`ModuleError::Signal`](crate::ModuleError::Signal), `(error "Canvas: No window system")` | `error` |
    ///
    /// [guide]: https://ubolonton.github.io/emacs-module-rs/latest/canvas.html
    pub fn with_canvas_data<F, R>(self, f: F) -> Result<R>
    where
        F: for<'a> FnOnce(CanvasData<'a>) -> R + Send,
    {
        let env = self.env;
        // Read the dimensions before `canvas_data`, because a `funcall` can run any Lisp code,
        // also for primitives (`post-gc-hook`, the debugger). After `canvas_data`, no `funcall`
        // happens, so nothing can invalidate the pointer before `f` returns. Keep a read error
        // for later, so that an invalid spec gives the error of `canvas_data` instead.
        let dimensions = self.canvas_dimensions();
        // XXX: Lisp code that runs after the first read, or inside `canvas_data` (file name
        // handlers for `:file`, `*Messages*` updates), can change the dimensions after we read
        // them. The module API cannot read a list without `funcall`, so we cannot close this
        // window. Lisp that redefines `cdr` or `plist-get` to return false values has the same
        // effect, because `use_functions!` resolves the definitions at module load. We do not
        // check for this: pass-through advice is harmless, and a check would break it. Fix:
        // Emacs's `canvas_data` returns the dimensions. See docs/research/canvas-data.md.
        let pointer = unsafe_raw_call!(env, canvas_data, self.raw)?;
        let (width, height) = dimensions?;
        if pointer.is_null() {
            unreachable!("Emacs failed to give canvas data but did not raise a signal");
        }
        // SAFETY:
        // - Length: `canvas_data` resized the buffer to the spec's current `:data-width` and
        //   `:data-height`. Emacs rejects specs with duplicate keys, and `plist-get` returns the
        //   first match, so these are the values that we read. This assumes that `cdr` and
        //   `plist-get` are the built-in primitives, and that no Lisp code changes the spec
        //   (see the XXX above for the gap).
        //   Emacs checked that `4 * width * height` fits in an `int`.
        // - Aliasing: Emacs holds no reference to the buffer while `f` runs. `f` cannot run Lisp
        //   code (`F: Send` keeps out `&Env` and `Value`), so nothing resizes, frees, or reads the
        //   buffer, and no nested call lends the same buffer twice.
        let buffer = unsafe { slice::from_raw_parts_mut(pointer, width * height) };
        Ok(f(CanvasData { buffer, width, height }))
    }

    fn canvas_dimensions(self) -> Result<(usize, usize)> {
        let env = self.env;
        let properties = env.call(subr::cdr, [self])?;
        let width = env.call(plist_get, (properties, data_width))?.into_rust()?;
        let height = env.call(plist_get, (properties, data_height))?.into_rust()?;
        Ok((width, height))
    }
}
