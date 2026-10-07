# Emacs 32 `canvas_data` module API

This doc records how the Emacs 32 canvas pixel buffer works, and which constraints a safe Rust binding must obey. Source: Emacs `master` at `1b85386156d` (2026-10-05).

- `src/module-env-32.h`: `canvas_data` declaration.
- `src/emacs-module.c`: `module_canvas_data`.
- `src/image.c`, section "Canvas": `struct canvas`, `canvas_get`, `canvas_free_unused`, `canvas_prepare_for_display`, `canvas_data`, `canvas-refresh`.
- `doc/lispref/display.texi` (Canvas Images), `doc/lispref/internals.texi` (Module Canvas API).

## Canvas identity

A canvas is an image spec with `:type canvas`, for example `(image :type canvas :id my-canvas :data-width 800 :data-height 600)`. `:id`, `:data-width` and `:data-height` are mandatory.

The spec cons object identifies the canvas. `canvas_map` is an `eq` hash table with weak keys. It maps each spec object to a `struct canvas`. A spec that is only `equal` to another spec gets a different canvas.

`canvas_get` is the single entry point. It creates the canvas if the spec has none. `canvas_data`, `canvas-refresh` and image loading during redisplay (`canvas_load`) all call it.

## Pixel buffer

| Property | Value |
|---|---|
| Allocation | `xzalloc` (malloc). It is not on the Lisp heap. |
| Element | `uint32_t`, native-endian, value `0xAARRGGBB` |
| Layout | Row-major, no padding, exactly `width * height` elements |
| Size limit | `width <= INT_MAX / 4 / height` |
| Alpha | Ignored on display. Cairo copies into an `RGB24` surface. X11 and Android mask off the alpha byte. |

`:data` strings and `:file` contents are byte arrays. On big-endian hosts, Emacs swaps them into native `u32` values.

The module API does not return the dimensions. They are only in the spec plist. A module can read them through `env` with `plist-get`.

## Pointer lifetime

GC does not move the buffer. The buffer is malloc memory, and this Emacs has no moving collector. Two events invalidate the pointer:

1. **Resize.** Lisp mutates `:data-width` or `:data-height` of the spec. On the next `canvas_get`, the dimensions do not match, so Emacs calls `xrealloc` and zeroes the buffer. Earlier pixels are lost.
2. **Free.** The spec becomes unreachable, and GC removes its entry from the weak `canvas_map`. Then `canvas_free_unused` frees the buffer. That function runs from `clear-image-cache` and before Emacs allocates a new canvas.

Consequences:

- `canvas_data` resizes the buffer to the spec's current dimensions before it returns. Read the dimensions from the same spec object immediately before the call, with no Lisp code between the two steps. Then the buffer length is always `width * height`.
- An `emacs_value` that a module holds is a GC root. Thus the buffer cannot be freed during a module call that holds the spec.
- Any `env` call that runs Lisp (`funcall` and similar) can mutate the spec, run redisplay, call `clear-image-cache`, or yield to another Lisp thread. The pointer can become invalid, or Emacs can read the buffer while the module writes to it. Simple `env` calls such as `make_integer` cannot cause this, but a borrow checker cannot easily tell the two kinds apart.

Rule: stop using the pointer before the next `env` call.

## Errors

`canvas_data` returns `NULL` and sets a pending non-local exit in these cases:

- The spec is not a valid canvas spec ("Not a canvas").
- Emacs was built without a window system ("Canvas: No window system").

A valid spec that has no canvas yet does not cause an error. Emacs creates the canvas, and loads `:data` or `:file` if the spec has them.

## Display

Writes to the buffer do not show until Lisp calls `canvas-refresh`. That function increments a refresh counter and marks the image glyphs for redraw. The next redisplay copies the buffer into the platform pixmap (`canvas_prepare_for_display`). When the call comes from a timer or a command, redisplay happens after it. In a loop, Lisp must call `redisplay` explicitly.

A module calls `canvas-refresh` through `funcall`. This is an `env` call, so the pointer must be out of use before it.

## Concurrency

Emacs reads the buffer only on the Lisp thread that holds the global lock, in redisplay and in `canvas-refresh`. No Emacs thread reads it at the same time as a module function runs.

To render from a background thread, use double buffering:

- The background thread writes into a buffer that Rust owns.
- A module function, called on the Lisp thread, copies that buffer into the canvas buffer, then calls `canvas-refresh`.
- The lock (or buffer swap) is only between the background thread and the Rust-owned buffer. Emacs does not need it. A swap or triple-buffer design avoids holding a lock during the copy.
- Compare dimensions before the copy. The canvas can be resized while the background thread renders a frame.

This is the first module API that gives a pointer to memory that Emacs owns. Earlier APIs use `emacs_value` handles, copy data into a buffer from the caller, or return the module's own pointer (`get_user_ptr`).
