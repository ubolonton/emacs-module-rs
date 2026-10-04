;;; -*- lexical-binding: t -*-

(require 'subr-x)
(require 'help)

(require 't)

(defvar t/support-module-assertions-p (> emacs-major-version 25))

(defvar t/support-bignum-p (fboundp 'bignump))

(defmacro t/get-error (&rest body)
  (declare (indent 0))
  `(condition-case err
       ,@body
     ('error err)))

(defun t/sig (sym)
  (let* ((docstring (documentation sym))
         (s (help-split-fundoc docstring sym)))
    (car s)))

(defun t/run-in-sub-process (f-symbol)
  (let* ((default-directory (getenv "PROJECT_ROOT"))
         (name (symbol-name f-symbol))
         (error-file (make-temp-file "destructive-fn"))
         (exit-code
          (apply #'call-process
                 (or (getenv "EMACS") "emacs") nil
                 (list t error-file)
                 nil
                 (append
                  (list "--batch"
                        "--directory" (getenv "MODULE_DIR"))
                  (when t/support-module-assertions-p '("--module-assertions"))
                  (list "-l" (expand-file-name "test-module/tests/main.el"
                                               (getenv "PROJECT_ROOT"))
                        "-f" name))))
         (error-string
          (with-temp-buffer
            (insert-file-contents error-file)
            (string-trim-right
             (buffer-substring-no-properties (point-min) (point-max))))))
    ;; When the process is terminated by a signal, `exit-code' is a string. Examples:
    ;; - SIGSEGV: GC bug 31238
    ;; - SIGABRT: `--module-assertions'
    (unless (equal exit-code 0)
      (error "Exit code: %s. Error: %s" exit-code error-string))))

(provide 't-helpers)
