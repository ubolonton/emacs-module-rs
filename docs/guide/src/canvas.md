# Canvas Images

`Value::with_canvas_data` lends your module the pixel buffer of an Emacs 32 canvas image. You draw into the buffer from Rust, then ask Lisp to show it.

## Setup

This feature is **experimental**. It tracks an unreleased module ABI.

Enable the feature:

```bash
cargo add emacs --features emacs-32-experimental
```

The module needs Emacs 32, built with a window system. Older Emacs versions refuse to load it. Without a window system, `with_canvas_data` signals `error`.

## Drawing

A canvas is an image spec with `:type canvas`. `:id`, `:data-width` and `:data-height` are mandatory. The spec object (not an `equal` copy) identifies the canvas.

```rust
{{#include ../examples/canvas.rs:fill}}
```

```elisp
(defvar my-canvas (list 'image :type 'canvas :id 'my-canvas :data-width 320 :data-height 240))
(insert-image my-canvas)
(my-module-fill my-canvas #xFF3366CC)
```

Emacs does not show the changes until Lisp calls `canvas-refresh` on the spec. In a loop, call `redisplay` as well.

## Rules

The closure receives a `CanvasData` with the fields `buffer`, `width` and `height`.

- `buffer` is a `&mut [u32]` with `width * height` elements. Pixels are in row-major order. Use `buffer.chunks_exact_mut(width)` to get rows.
- Each pixel is a native-endian `0xAARRGGBB` value. Emacs ignores alpha when it displays the canvas.
- A resize zeroes the buffer. Lisp resizes a canvas when it changes `:data-width` or `:data-height`. Pixels from before are lost.
- The closure must be `Send`, so it cannot capture `&Env` or `Value`. Thus it cannot call Lisp.

The last rule is the safety argument. Lisp code can resize or free the canvas, and redisplay reads the buffer. The buffer is valid only while no Lisp code runs. Call `canvas-refresh` after `with_canvas_data` returns, as `fill` does.

The bound also rejects other non-`Send` captures, such as `Rc`. Copy the data out of them first.

## Background Rendering

Emacs reads the buffer only on the Lisp thread. So a render thread must not hold the canvas buffer. Let the thread draw into a buffer that Rust owns, and swap it with a shared buffer. A defun, which runs on the Lisp thread, copies the shared buffer into the canvas.

```rust
{{#include ../examples/canvas_background.rs:example}}
```

```elisp
(my-module-render-start 320 240)
(run-with-timer 0 (/ 1.0 30) (lambda () (my-module-render-present my-canvas)))
```

- The render thread draws without the lock. It holds the lock only to swap, so a copy in `render_present` does not stop the drawing.
- The mutex guards only the Rust-owned buffers. Emacs needs no lock.
- `render_present` compares dimensions first. Lisp can resize the canvas while the thread renders a frame. Without the check, `copy_from_slice` panics on a length mismatch.

## Known Gap

If Lisp code that runs during the call resizes the same canvas, `buffer` has the wrong length. This code includes `post-gc-hook`, the debugger, file name handlers for `:file`, and `*Messages*` updates (`messages-buffer-mode` and its hooks) when Emacs reports an image error. Do not resize a canvas from such code.

The length is also wrong if Lisp redefines `cdr` or `plist-get` to return false values before the module loads. The module calls the definitions that exist at load time. Advice that passes the real results through, such as `trace-function`, is harmless.
