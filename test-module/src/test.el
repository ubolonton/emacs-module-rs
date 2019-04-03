(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'rs-module)
(require 't)

(ert-deftest convert::inc ()
  (should (= (t/inc 3) 4))
  (should (equal (documentation 't/inc) "1+"))

  (should-error (t/inc "3") :type 'wrong-type-argument)
  (should-error (t/inc nil) :type 'wrong-type-argument)

  (should-error (t/inc) :type 'wrong-number-of-arguments)
  (should-error (t/inc 1 2) :type 'wrong-number-of-arguments))

(ert-deftest convert::passthrough ()
  (let ((x "x"))
    (should (eq (t/identity x) x))
    (should (equal (documentation #'t/identity) "Return the input (not a copy)."))))

(ert-deftest convert::string ()
  (should (equal (t/to-uppercase "abc") "ABC")))

(ert-deftest convert::option-string ()
  (should (equal (t/to-lowercase-or-nil "CDE") "cde"))
  (should (equal (t/to-lowercase-or-nil nil) nil))
  (should-error (t/to-lowercase-or-nil 1) :type 'wrong-type-argument))

(ert-deftest error::propagating-signal ()
  (should-error (t/error:lisp-divide 1 0) :type 'arith-error))

(ert-deftest error::propagating-throw ()
  (should (let ((msg "Catch this!"))
            (eq (catch 'ball
                  (t/error:get-type
                   (lambda () (throw 'ball msg))))
                msg))))

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

(ert-deftest error::panic-parsing-arg ()
  (should-error (t/error:parse-arg 5 "1") :type 'rust-panic))

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

(ert-deftest transfer::vector ()

  (let* ((v1 (t/vector:make 5 6))
         (v2 (t/vector:make 1 3)))
    (should (string-prefix-p "#<user-ptr" (format "%s" v1)))
    (should (equal (t/vector:to-list v1) '(5 6)))
    (should (equal (t/vector:to-list
                    (t/vector:add v1 v2))
                   '(6 9)))
    ;; Emacs doesn't support custom equality...
    (should (not (equal (t/vector:add v1 v2)
                        (t/vector:add v1 v2))))
    ;; ... but should consider an object equal to itself.
    (should (let* ((v3 (t/vector:add v1 v2))
                   (v4 (t/identity v3)))
              (equal v3 v4))))

  ;; Mutation.
  (let ((v (t/vector:make 5 6)))
    (t/vector:scale-mutably 3 v)
    (should (equal (t/vector:to-list v) '(15 18)))
    (t/vector:swap-components v)
    (should (equal (t/vector:to-list v) '(18 15))))

  ;; ;; This is to trigger the finalizer. TODO: Somehow validate they actually runs.
  ;; (dotimes (i 100)
  ;;   (t/vector:make 1 2))
  ;; (garbage-collect)

  (let ((s (t/wrap-string "abc")))
    (should (string-prefix-p "#<user-ptr" (format "%s" s)))
    ;; TODO: Test 'rust-invalid-user-ptr. That probably requires 2 modules.
    (should-error (t/vector:to-list s) :type 'rust-wrong-type-user-ptr)))

(ert-deftest transfer::ref-cell-double-mutation ()
  (let ((r (t/ref-cell:make 5)))
    ;; FIX: Don't rely on error's string representation.
    (should (equal (condition-case err
                       (t/ref-cell:mutate-twice r)
                     (rust-error (format "%s" err)))
                   "(rust-error already borrowed)"))))

(ert-deftest transfer::type-check ()
  ;; TODO: :type
  (should-error (t/ref-cell:mutate-twice (t/vector:make 1 2)))
  (should-error (t/ref-cell:mutate-twice 5)))

(ert-deftest transfer::hash-map ()
  (let ((m (t/hash-map:make)))
    (should (equal (t/hash-map:get m "a") nil))

    (should (equal (t/hash-map:set m "a" "1") nil))
    (should (equal (t/hash-map:get m "a") "1"))

    (should (equal (t/hash-map:set m "a" "2") "1"))
    (should (equal (t/hash-map:get m "a") "2"))))

;;; Tests that, if failed, crash the whole process unrecoverably. They will be run under a
;;; sub-process Emacs.
(defmacro destructive-test (name)
  `(ert-deftest ,(intern (format "destructive::%s" name)) ()
     (let ((name ,(format "t/%s" name))
           (exit-code)
           (error-string)
           (error-file (make-temp-file "destructive-fn")))
       (setq exit-code (call-process
                        (format "%s/%s" (getenv "PROJECT_ROOT") "bin/fn.sh")
                        nil             ; no input
                        (list (if (getenv "VERBOSE")
                                  '(:file "/dev/stderr") ; ert prints stderr, not stdout.
                                t)
                              error-file)
                        t
                        name))
       (setq error-string (with-temp-buffer
                            (insert-file-contents error-file)
                            (goto-char (point-max))
                            (beginning-of-line 0)
                            (string-trim-right
                             (buffer-substring-no-properties (point) (point-max)))))
       (unless (= exit-code 0)
         (error "Exit code: %s. Error: %s" exit-code error-string)))))

(destructive-test gc-after-new-string)
(destructive-test gc-after-uninterning)
(destructive-test gc-after-retrieving)

;;; TODO: The way this test is called is a bit convoluted.
(defun t/gc-after-catching ()
  (t/gc-after-catching-1
   (lambda () (error "abc"))))
(destructive-test gc-after-catching)
