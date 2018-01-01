(require 'test-module)

(ert-deftest inc ()
  (should (= (test-module/inc 3) 4))
  (should (equal (documentation 'test-module/inc) "1+"))

  (should-error (test-module/inc "3") :type 'wrong-type-argument)
  (should-error (test-module/inc nil) :type 'wrong-type-argument)

  (should-error (test-module/inc) :type 'wrong-number-of-arguments)
  (should-error (test-module/inc 1 2) :type 'wrong-number-of-arguments)
  )

(ert-deftest propagate-errors ()
  (should-error (test-module/calling-error) :type 'arith-error))

(ert-deftest identity-eq ()
  :expected-result :failed
  (should (eq (test-module/identity "x") "x")))

(ert-deftest identity ()
  (should (equal (test-module/identity "x") "x")))

(ert-deftest create-function ()
  (let ((dec (test-module/make-dec)))
    (should (= (funcall dec 9) 8))
    (should (equal (documentation dec) "decrement"))
    ;; (should-error (func-call dec) :type 'wrong-number-of-arguments)

    ))

(ert-deftest from-emac-string ()
  (should (equal (test-module/to-uppercase "abc") "ABC")))
