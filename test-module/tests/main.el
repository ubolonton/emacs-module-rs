;;; Entry point for the integration tests. Load every `*-test.el' file in this directory.
;;;
;;; `t/run-in-sub-process' also loads this file, so that the sub-process sees the functions defined
;;; by the test files.

(defvar t/tests-dir (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path t/tests-dir)

(require 'rs-module)
(require 't-helpers)

(dolist (file (directory-files t/tests-dir t "-test\\.el\\'"))
  (load file nil t))
