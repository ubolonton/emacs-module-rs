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

(ert-deftest error::rust-invalid-utf-8 ()
  (let ((s "\377"))                     ; A unibyte string. Its byte is not valid UTF-8.
    (should (equal (t/error:variant "string" s nil) "Rust/InvalidUtf8"))
    ;; Conversion to bytes does not check UTF-8.
    (should (equal (t/error:variant "bytes" s nil) nil))
    (t/should-signal (t/to-uppercase s)
      'rust-invalid-utf-8 '(rust-error wrong-type-argument) (list 'utf-8-string-p s))))

(ert-deftest error::module-wrong-type ()
  (dolist (case '(("i64" "3" "Module/WrongType/Integer")
                  ("f64" "x" "Module/WrongType/Float")
                  ("string" 5 "Module/WrongType/String")
                  ("vector" 5 "Module/WrongType/Vector")
                  ("ref-cell" 5 "Module/WrongType/UserPtr")))
    (should (equal (t/error:variant (nth 0 case) (nth 1 case) nil) (nth 2 case))))
  (t/should-signal (t/inc "3")
    'rust-module-wrong-type '(rust-module-error wrong-type-argument) '(integerp "3"))
  ;; Emacs 25 says `user-ptr'. The crate always says `user-ptrp'.
  (t/should-signal (t/transfer-ref-cell-inc 5)
    'rust-module-wrong-type '(rust-module-error wrong-type-argument) '(user-ptrp 5))
  ;; Printed errors do not change.
  (should (equal (error-message-string (t/get-error (t/inc "3")))
                 "Wrong type argument: integerp, \"3\"")))

(ert-deftest error::module-buffer-too-small ()
  (should (equal (t/error:variant "copy-string-contents" "xyz" 3) "Module/BufferTooSmall"))
  ;; Emacs 31 signals `memory-buffer-too-small'. Earlier versions signal `args-out-of-range'.
  (let ((parents (append '(rust-module-error args-out-of-range)
                         (when (>= emacs-major-version 31) '(memory-buffer-too-small)))))
    ;; The data is (ACTUAL REQUIRED). The required size includes the null terminator.
    (t/should-signal (t/conversion-copy-string-contents "xyz" 3)
      'rust-module-buffer-too-small parents '(3 4))
    (t/should-signal (t/conversion-copy-string-contents "" 0)
      'rust-module-buffer-too-small parents '(0 1))))

(ert-deftest error::module-non-unicode-string ()
  ;; Emacs 25 and 26 do not check this.
  (skip-unless (>= emacs-major-version 27))
  (let ((s (string #x3FFFFF)))          ; Raw byte FF, in a multibyte string.
    (should (equal (t/error:variant "bytes" s nil) "Module/NonUnicodeString"))
    (should (equal (t/error:variant "string" s nil) "Module/NonUnicodeString"))
    (t/should-signal (t/conversion-string-to-bytes s)
      'rust-module-non-unicode-string '(rust-module-error wrong-type-argument)
      (list 'unicode-string-p s))))

(ert-deftest error::module-index-out-of-range ()
  (let ((v [0 1 2 3]))
    (should (equal (t/error:variant "vec-get" v -1) "Module/IndexOutOfRange"))
    ;; Emacs 25 signals `overflow-error' for an index outside the fixnum range.
    (should (equal (t/error:variant "vec-get-far" v nil) "Module/IndexOutOfRange"))
    ;; The data is (VECTOR INDEX) on all versions.
    (t/should-signal (t/conversion-vec-get v -1)
      'rust-module-index-out-of-range '(rust-module-error args-out-of-range) (list v -1))
    (t/should-signal (t/conversion-vec-set v 4 'a)
      'rust-module-index-out-of-range '(rust-module-error args-out-of-range) (list v 4))))

(ert-deftest error::module-integer-out-of-range ()
  (let ((parents '(rust-module-error overflow-error)))
    (if t/support-bignum-p
        (let ((big (expt 2 64)))
          (should (equal (t/error:variant "i64" big nil) "Module/IntegerOutOfRange"))
          (t/should-signal (t/inc big) 'rust-module-integer-out-of-range parents (list big))
          ;; With bignums, `make_integer' does not fail.
          (should (equal (t/error:variant "i64-max-into-lisp" nil nil) nil)))
      ;; Without bignums, `make_integer' fails for an `i64' outside the fixnum range. No Lisp
      ;; value exists for it, so the data is empty.
      (should (equal (t/error:variant "i64-max-into-lisp" nil nil) "Module/IntegerOutOfRange"))
      (t/should-signal (t/inc most-positive-fixnum)
        'rust-module-integer-out-of-range parents nil))))
