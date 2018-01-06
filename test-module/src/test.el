(require 'test-module)

(ert-deftest inc ()
  (should (= (test-module/inc 3) 4))
  (should (equal (documentation 'test-module/inc) "1+"))

  (should-error (test-module/inc "3") :type 'wrong-type-argument)
  (should-error (test-module/inc nil) :type 'wrong-type-argument)

  (should-error (test-module/inc) :type 'wrong-number-of-arguments)
  (should-error (test-module/inc 1 2) :type 'wrong-number-of-arguments))

(ert-deftest propagate-errors ()
  (should-error (test-module/calling-error) :type 'arith-error))

(ert-deftest passthrough ()
  (let ((x "x"))
    (should (eq (test-module/identity x) x))))

(ert-deftest create-function ()
  (let ((dec (test-module/make-dec)))
    (should (= (funcall dec 9) 8))
    (should (equal (documentation dec) "decrement"))
    (should-error (funcall dec) :type 'wrong-number-of-arguments)))

(ert-deftest from-emac-string ()
  (should (equal (test-module/to-uppercase "abc") "ABC")))

(ert-deftest user-ptr ()

  (let ((p (test-module/make-point 5 6)))
    (should (string-prefix-p "#<user-ptr" (format "%s" p)))
    (should (= (test-module/check-point p) 11)))

  ;; ;; This is to trigger the finalizer. TODO: Somehow validate they actually runs.
  ;; (dotimes (i 100)
  ;;   (test-module/make-point 1 2))
  ;; (garbage-collect)

  (let ((s (test-module/wrap-string "abc")))
    (should (string-prefix-p "#<user-ptr" (format "%s" s)))
    ;; TODO: :type
    (should-error (test-module/check-point s))))
