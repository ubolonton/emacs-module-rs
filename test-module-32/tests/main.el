;;; -*- lexical-binding: t -*-
;;; ----------------------------------------------------------------------------
;;; ABI compatibility. `t32' is built with the `emacs-32-experimental' feature, so it must refuse
;;; to load on an older Emacs instead of loading and later reading past the end of its (smaller)
;;; `emacs_env' struct.

(if (>= emacs-major-version 32)
    (require 't32)
  (ert-deftest module-load::rejects-incompatible-abi ()
    (should-error (require 't32))))
