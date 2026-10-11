;;; -*- lexical-binding: t -*-
;;; Source locations of module functions.

(require 't-helpers)

(defvar t/source-root (file-name-directory (directory-file-name t/tests-dir))
  "Root of the test module's crate.")

(defun t/source-line (file text)
  "Return the line where TEXT starts in FILE, relative to `t/source-root'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file t/source-root))
    (search-forward text)
    (line-number-at-pos (match-beginning 0))))

(ert-deftest source::location ()
  ;; Starts at the docstring above `#[defun]'.
  (should (equal (get 't/inc 'emacs-module-rs-source)
                 `("src/lib.rs" ,(t/source-line "src/lib.rs" "/// 1+\n#[defun]\nfn inc(") t)))
  ;; Starts at `#[defun]', above the docstring.
  (should (equal (get 't/identity 'emacs-module-rs-source)
                 `("src/lib.rs" ,(t/source-line "src/lib.rs" "#[defun]\n///Return the input") t)))
  (should (equal (get 't/source-attributes-above 'emacs-module-rs-source)
                 `("src/source.rs" ,(t/source-line "src/source.rs" "/// A docstring, then") t)))
  (should (equal (get 't/source-no-attributes 'emacs-module-rs-source)
                 `("src/source.rs" ,(t/source-line "src/source.rs" "#[defun]\nfn no_attributes") t)))
  ;; Not defined by `#[defun]'.
  (should-not (get 't/sum-and-diff 'emacs-module-rs-source)))
