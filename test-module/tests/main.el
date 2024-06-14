(require 'subr-x)
(require 'help)

(require 'rs-module)
(require 't)

;;; ----------------------------------------------------------------------------
;;; Test helpers.

(defvar t/support-module-assertions-p (> emacs-major-version 25))

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
          (pcase system-type
            ((or 'darwin 'gnu/linux 'berkeley-unix)
             (call-process
              "bash"
              ;; If VERBOSE, redirect subprocess's stdout to stderr
              nil (list (if (getenv "VERBOSE")
                            '(:file "/dev/stderr")
                          t)
                        error-file)
              nil (if t/support-module-assertions-p
                      "./bin/fn-module-assertions"
                    "./bin/fn") name))
            ('windows-nt
             (call-process
              "powershell"
              ;; If VERBOSE, redirect subprocess's stdout to stderr
              nil (list t error-file)
              nil (if t/support-module-assertions-p
                      ".\\bin\\fn-module-assertions.ps1"
                    ".\\bin\\fn.ps1") name))
            (_ (error "Unsupported system-type: %s" system-type))))
         (error-string
          (with-temp-buffer
            (insert-file-contents error-file)
            (string-trim-right
             (buffer-substring-no-properties (point-min) (point-max))))))
    (unless (= exit-code 0)
      (error "Exit code: %s. Error: %s" exit-code error-string))))

;;; ----------------------------------------------------------------------------
;;; Type conversion.

(ert-deftest conversion::integers ()
  (should (= (t/inc 3) 4))
  (should (string-match-p (regexp-quote "1+")
                          (documentation 't/inc) ))

  (should-error (t/inc "3") :type 'wrong-type-argument)
  (should-error (t/inc nil) :type 'wrong-type-argument)

  (should-error (t/inc) :type 'wrong-number-of-arguments)
  (should-error (t/inc 1 2) :type 'wrong-number-of-arguments)

  (should (= -128 (t/identity-i8 -128)))
  (should (= 255 (t/identity-u8 255)))

  ;; FIX: Don't rely on error's string representation.
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/u64-overflow) :type 'rust-error))))
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/identity-i8 128) :type 'rust-error))))
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/identity-u8 -1) :type 'rust-error)))))

(ert-deftest conversion::passthrough ()
  (let ((x "x"))
    (should (eq (t/identity x) x))
    (should (eq (t/identity 5) 5))
    (should (string-match-p (regexp-quote "Return the input (not a copy).")
                            (documentation #'t/identity) ))))

(ert-deftest conversion::string ()
  (should (equal (t/to-uppercase "abc") "ABC"))
  ;; copy_string_contents copies the null terminator.
  (should-error (t/copy-string-contents "xyz" 3) :type 'args-out-of-range)
  (should-error (t/copy-string-contents "" 0) :type 'args-out-of-range)
  (should (string= "xyz" (t/copy-string-contents "xyz" 4)))
  (should (string= "" (t/copy-string-contents "" 1)))
  (should-error (t/copy-string-contents "abcxyz" 3) :type 'args-out-of-range))

(ert-deftest conversion::option-string ()
  (should (equal (t/to-lowercase-or-nil "CDE") "cde"))
  (should (equal (t/to-lowercase-or-nil nil) nil))
  (should-error (t/to-lowercase-or-nil 1) :type 'wrong-type-argument))

(ert-deftest conversion::vector-functions ()
  (should (equal (t/make-vector 5 nil) (make-vector 5 nil)))
  (let ((v [0 1 2 3]))
    (should (= 4 (t/vec-size v)))
    (t/vec-set v 2 'a)
    (should (eq 'a (t/vec-get v 2)))
    (should-error (t/vec-get v -1) :type 'args-out-of-range)
    (should-error (t/vec-set v -5 'a) :type 'args-out-of-range))
  (let ((v [a b c d e]))
    (should (eq v (t/identity-if-vector v)))
    (should-error (t/identity-if-vector nil) :type 'wrong-type-argument)
    (should (equal (t/get-error (eq "abc" (t/identity-if-vector "abc")))
                   '(wrong-type-argument vectorp "abc"))))
  (let ((v [0 1 2 3]))
    (should (eq v (t/stringify-num-vector v)))
    (should (equal v ["0" "1" "2" "3"]))
    (should-error (t/stringify-num-vector v) :type 'wrong-type-argument)))

;;; ----------------------------------------------------------------------------
;;; Non-local exits.

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

;;; ----------------------------------------------------------------------------
;;; Functions.

(ert-deftest calling::through-env ()
  (should (equal '(0 1 2) (t/call-list 3))))

(ert-deftest calling::through-value ()
  (should (eq 'integer (t/call-value 'type-of 3)))
  (should (equal "xyz" (t/call-value (symbol-function 'symbol-name) 'xyz)))
  (should (eq 'abc (t/call-value (lambda (x) x) 'abc)))
  (should-error (t/call-value nil nil) :type 'void-function)
  (should-error (t/call-value 3 nil) :type 'invalid-function))

(ert-deftest function::create ()
  (let ((dec (t/make-dec)))
    (should (= (funcall dec 9) 8))
    (should (equal (documentation dec) "decrement"))
    (should-error (funcall dec) :type 'wrong-number-of-arguments))
  (let* ((fns (t/make-inc-and-plus))
         (inc (car fns))
         (plus (cdr fns)))
    (should (= (funcall inc -2) -1))
    (should (equal (documentation inc) "increment"))
    (should-error (funcall inc) :type 'wrong-number-of-arguments)

    (should (= (funcall plus 3 5) 8))
    (should (equal (documentation plus) ""))
    (should-error (funcall plus "s" 5) :type 'wrong-type-argument)))

(ert-deftest function::return-type-auto-coercion ()
  (let ((x 3)
        (y 4))
    (should (equal (t/sum x y)
                   (+ x y)))))

(ert-deftest function::fset ()
  (let ((x 5)
        (y 3))
    (should (equal (t/sum-and-diff x y)
                   (list (+ x y) (- x y))))))

(ert-deftest function::defun-signature ()
  (should (equal (t/sig 't/ignore-args)
                 "(t/ignore-args _ _)"))
  (should (equal (t/sig 't/to-lowercase-or-nil)
                 "(t/to-lowercase-or-nil INPUT)"))
  (should (equal (t/sig 't/error:catch)
                 "(t/error:catch EXPECTED-TAG LAMBDA)")))

;;; ----------------------------------------------------------------------------
;;; user-ptr.

(ert-deftest transfer::vector ()

  (let* ((v1 (t/vector-make 5 6))
         (v2 (t/vector-make 1 3)))
    (should (string-prefix-p "#<user-ptr" (format "%s" v1)))
    (should (equal (t/vector-to-list v1) '(5 6)))
    (should (equal (t/vector-to-list
                    (t/vector-add v1 v2))
                   '(6 9)))
    ;; Emacs doesn't support custom equality...
    (should (not (equal (t/vector-add v1 v2)
                        (t/vector-add v1 v2))))
    ;; ... but should consider an object equal to itself.
    (should (let* ((v3 (t/vector-add v1 v2))
                   (v4 (t/identity v3)))
              (equal v3 v4))))

  ;; Mutation.
  (let ((v (t/vector-make 5 6)))
    (t/vector-scale-mutably 3 v)
    (should (equal (t/vector-to-list v) '(15 18)))
    (t/vector-swap-components v)
    (should (equal (t/vector-to-list v) '(18 15))))

  ;; ;; This is to trigger the finalizer. TODO: Somehow validate they actually runs.
  ;; (dotimes (i 100)
  ;;   (t/vector-make 1 2))
  ;; (garbage-collect)

  (let ((s (t/wrap-string "abc")))
    (should (string-prefix-p "#<user-ptr" (format "%s" s)))
    ;; TODO: Test 'rust-invalid-user-ptr. That probably requires 2 modules.
    (should-error (t/vector-to-list s)
                  :type 'rust-wrong-type-user-ptr)))

(ert-deftest transfer::ref-cell-borrow-conflict ()
  (let ((r (t/ref-cell-wrap 5)))
    (should (= (t/ref-cell-inc r) 6))
    (should (= (t/ref-cell-unwrap r) 6))
    (t/ref-cell-unwrap-and-call r (lambda () (t/ref-cell-unwrap r)))
    ;; FIX: Don't rely on error's string representation.
    (should (equal (cdr (should-error (t/ref-cell-unwrap-and-call r (lambda () (t/ref-cell-inc r)))
                                      :type 'rust-error))
                   '("already borrowed")))))

(ert-deftest transfer::type-check ()
  (should-error (t/ref-cell-inc (t/vector-make 1 2))
                :type 'rust-wrong-type-user-ptr)
  (should-error (t/ref-cell-inc 5)
                :type 'wrong-type-argument)
  (ert-info ("'rust-wrong-type-user-ptr should be both 'rust-error and 'wrong-type-argument")
    (let ((parent-symbols (get 'rust-wrong-type-user-ptr 'error-conditions)))
      (should (member 'rust-error parent-symbols))
      (should (member 'wrong-type-argument parent-symbols)))
    (should-error (t/ref-cell-inc (t/vector-make 1 2)) :type 'rust-error)
    (should-error (t/ref-cell-inc (t/vector-make 1 2)) :type 'wrong-type-argument)))

(ert-deftest transfer::hash-map ()
  (let ((m (t/hash-map-make)))
    (should (equal (t/hash-map-get m "a") nil))

    (should (equal (t/hash-map-set m "a" "1") nil))
    (should (equal (t/hash-map-get m "a") "1"))

    (should (equal (t/hash-map-set m "a" "2") "1"))
    (should (equal (t/hash-map-get m "a") "2"))))

;;; ----------------------------------------------------------------------------
;;; Memory safety tests.

;;; Tests that, if failed, crash the whole process unrecoverably. They will be run under a
;;; sub-process Emacs.
(defmacro destructive-test (name &optional prefix)
  `(ert-deftest ,(intern (if prefix
                             (format "%s::%s" prefix name)
                           (format "%s" name))) ()
     (t/run-in-sub-process (intern ,(format "t/%s" name)))))

;;; TODO: The way this test is called is a bit convoluted.
(defun t/gc-after-catching ()
  (t/gc-after-catching-1
   (lambda () (error "abc"))))

(destructive-test gc-after-new-string lifetime)
(destructive-test gc-after-new-int lifetime)
(destructive-test gc-after-new-float lifetime)
(destructive-test gc-after-uninterning lifetime)
(destructive-test gc-after-retrieving lifetime)
(destructive-test gc-after-catching lifetime)

;;; ----------------------------------------------------------------------------
;;; Memory leak tests.

(defun t/free-global-ref-after-normal-return ()
  (t/trigger-double-free-global-ref
   (lambda ())))

(defun t/free-global-ref-after-error ()
  (t/trigger-double-free-global-ref
   (lambda () (error "This should not show up because Emacs should crash when run with --module-assertions"))))

(ert-deftest global-ref::free-after-normal-return ()
  (unless t/support-module-assertions-p
    (ert-skip "--module-assertions is not supported"))
  (when (eq system-type 'windows-nt)
    (ert-skip "We don't know how to correctly handle failed PowerShell subprocess"))
  (when (bound-and-true-p module-rs-disable-gc-bug-31238-workaround)
    (ert-skip "Workaround for the GC bug 31238 was already disabled"))
  (should (string-match-p
           "Emacs value not found in"
           (cadr (t/get-error (t/run-in-sub-process 't/free-global-ref-after-normal-return))))))

(ert-deftest global-ref::free-after-error ()
  (unless t/support-module-assertions-p
    (ert-skip "--module-assertions is not supported"))
  (when (eq system-type 'windows-nt)
    (ert-skip "We don't know how to correctly handle failed PowerShell subprocess"))
  (when (bound-and-true-p module-rs-disable-gc-bug-31238-workaround)
    (ert-skip "Workaround for the GC bug 31238 was already disabled"))
  (should (string-match-p
           "Emacs value not found in"
           (cadr (t/get-error (t/run-in-sub-process 't/free-global-ref-after-error))))))
