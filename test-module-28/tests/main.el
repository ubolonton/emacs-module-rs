;;; -*- lexical-binding: t -*-
;;; ----------------------------------------------------------------------------
;;; ABI compatibility. `t28` is built with the `emacs-28` feature, so it must refuse to load on an
;;; older Emacs instead of loading and later reading past the end of its (smaller) `emacs_env`
;;; struct.

(if (>= emacs-major-version 28)
    (require 't28)
  (ert-deftest module-load::rejects-incompatible-abi ()
    (should-error (require 't28))))

;;; ----------------------------------------------------------------------------
;;; open-channel tests (Emacs 28+).

(ert-deftest channel::send-and-receive ()
  (skip-unless (>= emacs-major-version 28))
  (let* ((received nil)
         (proc (make-pipe-process
                :name "test-send"
                :filter (lambda (_proc output)
                          (setq received (concat received output))))))
    (unwind-protect
        (progn
          (t28/channel-send proc "hello from rust")
          (accept-process-output proc 5)
          (should (equal received "hello from rust")))
      (delete-process proc))))

(ert-deftest channel::send-from-thread ()
  (skip-unless (>= emacs-major-version 28))
  (let* ((received nil)
         (proc (make-pipe-process
                :name "test-thread-send"
                :filter (lambda (_proc output)
                          (setq received (concat received output))))))
    (unwind-protect
        (progn
          (t28/channel-send-from-thread proc "threaded hello")
          (accept-process-output proc 5)
          (should (equal received "threaded hello")))
      (delete-process proc))))

(ert-deftest channel::wrong-type-arg ()
  (skip-unless (>= emacs-major-version 28))
  (let ((err (should-error (t28/channel-send "not-a-process" "data")
                           :type 'rust-module-wrong-type)))
    (should (memq 'wrong-type-argument (get (car err) 'error-conditions)))
    (should (equal (cdr err) '(processp "not-a-process"))))
  ;; A network process is a process, but not a pipe process.
  (let ((proc (make-network-process :name "t28-not-pipe" :server t :service t
                                    :host 'local :noquery t)))
    (unwind-protect
        (let ((err (should-error (t28/channel-send proc "data")
                                 :type 'rust-module-wrong-type)))
          (should (equal (cdr err) (list 'pipe-process-p proc))))
      (delete-process proc))))

;;; ----------------------------------------------------------------------------
;;; Byte chunks to unibyte string (Emacs 28+).

(ert-deftest conversion::bytes-roundtrip ()
  (skip-unless (>= emacs-major-version 28))
  (dolist (s '("\377\0abc" "abc" ""))
    (dolist (f '(t28/bytes-roundtrip t28/bytes-roundtrip-boxed))
      (let ((result (funcall f s)))
        (should (equal result s))
        (should-not (multibyte-string-p result))))))

(ert-deftest conversion::bytes-into-lisp ()
  (skip-unless (>= emacs-major-version 28))
  (dolist (f '(t28/bytes-static t28/bytes-literal t28/bytes-borrowed-vec))
    (let ((result (funcall f)))
      (should (equal result "\377\0"))
      (should-not (multibyte-string-p result))))
  (let ((result (t28/bytes-all)))
    (should (equal result (apply #'unibyte-string (number-sequence 0 255))))
    (should-not (multibyte-string-p result))))
