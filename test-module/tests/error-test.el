;;; -*- lexical-binding: t -*-
;;; Non-local exits.

(require 't-helpers)

(ert-deftest error::propagating-signal ()
  ;; Through Result.
  (should-error (t/error:lisp-divide 1 0) :type 'arith-error)
  ;; Through panic.
  (should-error (t/error:apply #'/ '(1 0)) :type 'arith-error)
  (should-error (t/error:apply (lambda () (error "abc")) nil) :type 'error))

(ert-deftest error::propagating-throw ()
  (let ((msg "Catch this!"))
    ;; Through Result.
    (should (eq (catch 'ball
                  (t/error:get-type
                   (lambda () (throw 'ball msg))))
                msg))
    ;; Through panic.
    (should (eq (catch 'knife
                  (t/error:apply
                   (lambda () (throw 'knife msg))
                   nil))
                msg))))

(ert-deftest error::wrap-signal ()
  (should (> (length (t/read-file "Cargo.toml")) 0))
  (should-error (t/read-file "!@#%%&") :type 'emrs-file-error))

(ert-deftest error::handling-signal ()
  (should (eq (t/error:get-type (lambda () (error "?"))) 'error))
  (should (eq (t/error:get-type (lambda () (user-error "?"))) 'user-error)))

(ert-deftest error::handling-throw ()
  (should (let ((msg "Catch this!"))
            (eq (t/error:catch 'ball
                               (lambda () (throw 'ball msg)))
                msg)))
  (should-error (t/error:catch 'ball
                               (lambda () (throw 'knife "Watch out!")))
                :type 'no-catch))

(ert-deftest error::panic ()
  (should-error (t/error:parse-arg 5 "1") :type 'rust-panic)
  (should (equal (t/get-error (t/error:apply #'t/error:panic '("abc")))
                 '(rust-panic "abc"))))

(ert-deftest error::signal ()
  (should-error (t/error:signal 'rust-error "BAZ") :type 'rust-error)
  (condition-case err
      (t/error:signal 'rust-error "abc")
    (rust-error (should (equal err '(rust-error . ("abc"))))))
  (should-error (t/error:signal-custom) :type 'emacs-module-rs-test-error)
  (should-error (t/error:signal-custom) :type 'rust-error)
  (should-error (t/error:signal-custom) :type 'error)
  (condition-case err
      (t/error:signal 'emacs-module-rs-test-error "abc")
    (rust-error (should (equal err '(emacs-module-rs-test-error . ("abc"))))))
  (should-error (signal 'error-defined-without-parent nil) :type 'error))

(ert-deftest error::variant-lisp-code ()
  ;; Exits of Lisp code stay `Signal' and `Throw', also for symbols that the module layer uses.
  (should (equal (t/error:variant
                  "funcall" (lambda () (signal 'wrong-type-argument '(integerp "3"))) nil)
                 "Signal"))
  (should (equal (t/error:variant "funcall" (lambda () (throw 'ball 1)) nil) "Throw"))
  (should (equal (t/error:variant "funcall" (lambda () 1) nil) nil)))

(ert-deftest error::variant-module-layer ()
  ;; Signals from module functions other than `funcall' are module-layer errors.
  (should (string-prefix-p "Module/" (t/error:variant "i64" "3" nil))))
