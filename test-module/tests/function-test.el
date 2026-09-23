;;; Functions.

(require 't-helpers)

(ert-deftest calling::through-env ()
  (should (equal '(0 1 2) (t/function-call-list 3))))

(ert-deftest calling::through-value ()
  (should (eq 'integer (t/function-call-value 'type-of 3)))
  (should (equal "xyz" (t/function-call-value (symbol-function 'symbol-name) 'xyz)))
  (should (eq 'abc (t/function-call-value (lambda (x) x) 'abc)))
  (should-error (t/function-call-value nil nil) :type 'void-function)
  (should-error (t/function-call-value 3 nil) :type 'invalid-function))

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
  (should (equal (t/sig 't/function-ignore-args)
                 "(t/function-ignore-args _ _)"))
  (should (equal (t/sig 't/conversion-to-lowercase-or-nil)
                 "(t/conversion-to-lowercase-or-nil INPUT)"))
  (should (equal (t/sig 't/error:catch)
                 "(t/error:catch EXPECTED-TAG LAMBDA)")))
