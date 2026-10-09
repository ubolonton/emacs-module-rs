;;; -*- lexical-binding: t -*-
;;; ----------------------------------------------------------------------------
;;; ABI compatibility. `t32' is built with the `emacs-32-experimental' feature, so it must refuse
;;; to load on an older Emacs instead of loading and later reading past the end of its (smaller)
;;; `emacs_env' struct.

(if (>= emacs-major-version 32)
    (require 't32)
  (ert-deftest module-load::rejects-incompatible-abi ()
    (should-error (require 't32))))

;;; ----------------------------------------------------------------------------
;;; Canvas tests (Emacs 32+). `image.c' is compiled only with a window system, so `canvas-refresh'
;;; tells whether this Emacs supports canvases. GTK builds support them in batch mode.

(defun t32--canvas (width height &rest properties)
  "Return a new canvas spec of WIDTH x HEIGHT pixels, with extra PROPERTIES.
Each call returns a new list, so each call makes a new canvas."
  (append (list 'image :type 'canvas :id 't32-canvas :data-width width :data-height height)
          properties))

(ert-deftest canvas::dimensions ()
  (skip-unless (fboundp 'canvas-refresh))
  (should (equal (t32/canvas-fill (t32--canvas 8 4) #xFF00FF00) '(8 4 32))))

(ert-deftest canvas::write-persists ()
  (skip-unless (fboundp 'canvas-refresh))
  (let ((canvas (t32--canvas 8 4)))
    (t32/canvas-fill canvas #xFF112233)
    (should (equal (t32/canvas-pixel canvas 0) #xFF112233))
    (should (equal (t32/canvas-pixel canvas 31) #xFF112233))
    (should-not (t32/canvas-pixel canvas 32))))

(ert-deftest canvas::initial-data ()
  (skip-unless (fboundp 'canvas-refresh))
  ;; A vector holds pixel values.
  (let* ((data (vector #xFF000000 #x00FF0000 #x0000FF00 #x000000FF #x12345678 #x87654321))
         (canvas (t32--canvas 3 2 :data data)))
    (dotimes (index (length data))
      (should (equal (t32/canvas-pixel canvas index) (aref data index)))))
  ;; A unibyte string holds little-endian bytes, on all hosts.
  (let ((canvas (t32--canvas 1 1 :data (unibyte-string #x78 #x56 #x34 #x12))))
    (should (equal (t32/canvas-pixel canvas 0) #x12345678))))

(ert-deftest canvas::resize ()
  (skip-unless (fboundp 'canvas-refresh))
  (let ((canvas (t32--canvas 8 4)))
    (t32/canvas-fill canvas #xFFFFFFFF)
    (plist-put (cdr canvas) :data-width 16)
    ;; Emacs zeroes the buffer when it resizes it.
    (should (equal (t32/canvas-pixel canvas 0) 0))
    (should (equal (t32/canvas-fill canvas 1) '(16 4 64)))))

(ert-deftest canvas::not-a-canvas ()
  (skip-unless (fboundp 'canvas-refresh))
  (dolist (spec '((image :type png :file "x.png") 42))
    (let ((err (should-error (t32/canvas-fill spec 0))))
      (should (equal err '(error "Not a canvas"))))))

(ert-deftest canvas::uninterned-dimension-key ()
  (skip-unless (fboundp 'canvas-refresh))
  ;; Emacs matches spec keys by name, so `canvas_data' accepts an uninterned `:data-width'. The
  ;; binding reads dimensions with `plist-get', which compares with `eq' and finds nothing.
  (let* ((spec (list 'image :type 'canvas :id 't32-canvas
                     (make-symbol ":data-width") 8 :data-height 4))
         (err (should-error (t32/canvas-fill spec 0))))
    (should (equal err '(rust-module-wrong-type integerp nil)))))

(ert-deftest canvas::no-window-system ()
  (skip-unless (and (>= emacs-major-version 32) (not (fboundp 'canvas-refresh))))
  (let ((err (should-error (t32/canvas-fill (t32--canvas 8 4) 0))))
    (should (equal err '(error "Canvas: No window system")))))
