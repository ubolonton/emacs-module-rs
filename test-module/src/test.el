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

  (let* ((v1 (test-module/make-vector 5 6))
         (v2 (test-module/make-vector 1 3)))
    (should (string-prefix-p "#<user-ptr" (format "%s" v1)))
    (should (equal (test-module/vector-to-list v1) '(5 6)))
    (should (equal (test-module/vector-to-list
                    (test-module/add-vectors v1 v2))
                   '(6 9)))
    ;; Emacs doesn't support custom equality...
    (should (not (equal (test-module/add-vectors v1 v2)
                        (test-module/add-vectors v1 v2))))
    ;; ... but should consider an object equal to itself.
    (should (let* ((v3 (test-module/add-vectors v1 v2))
                   (v4 (test-module/identity v3)))
              (equal v3 v4)))

    (test-module/scale-vector-mutably 3 v1)
    (should (equal (test-module/vector-to-list v1)
                   '(15 18)))
    )

  ;; ;; This is to trigger the finalizer. TODO: Somehow validate they actually runs.
  ;; (dotimes (i 100)
  ;;   (test-module/make-vector 1 2))
  ;; (garbage-collect)

  (let ((s (test-module/wrap-string "abc")))
    (should (string-prefix-p "#<user-ptr" (format "%s" s)))
    ;; TODO: :type
    (should-error (test-module/vector-to-list s))))
