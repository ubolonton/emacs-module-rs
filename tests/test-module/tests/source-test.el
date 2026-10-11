;;; -*- lexical-binding: t -*-
;;; Source locations of module functions.

(require 'find-func)
(require 'elisp-mode)
(require 'xref)
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

(defmacro t/with-source-root (&rest body)
  "Run BODY with the test module's source root set."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (put 't 'emacs-module-rs-source-root t/source-root)
         ,@body)
     (put 't 'emacs-module-rs-source-root nil)))

(defun t/source-should-visit (location file text)
  "Check that LOCATION, a (BUFFER . POSITION), is at TEXT in FILE."
  (with-current-buffer (car location)
    (should (file-equal-p buffer-file-name (expand-file-name file t/source-root)))
    (goto-char (cdr location))
    (should (looking-at-p (regexp-quote text)))))

(ert-deftest source::find-function ()
  (t/with-source-root
    (t/source-should-visit (find-function-noselect 't/inc) "src/lib.rs" "/// 1+\n#[defun]")
    (t/source-should-visit (find-function-noselect 't/source-attributes-above)
                           "src/source.rs" "/// A docstring, then")))

(ert-deftest source::xref ()
  (t/with-source-root
    (let* ((xrefs (xref-backend-definitions 'elisp "t/identity"))
           (marker (xref-location-marker (xref-item-location (car xrefs)))))
      (t/source-should-visit (cons (marker-buffer marker) (marker-position marker))
                             "src/lib.rs" "#[defun]\n///Return the input"))))

(ert-deftest source::without-root ()
  (should (get 't/inc 'emacs-module-rs-source))
  (should-not (emacs-module-rs--find-source 't/inc)))
