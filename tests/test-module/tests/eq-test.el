;;; -*- lexical-binding: t -*-
;;; Value equality (PartialEq).

(require 't-helpers)

(ert-deftest eq::value-same-object ()
  "Value == Value is true when they refer to the same object."
  ;; Symbols are interned, so two symbols of the same name are eq.
  (should (t/eq:value-eq 'hello 'hello))
  (should (t/eq:value-eq () nil))
  ;; Small integers (fixnum) are "inlined".
  (should (t/eq:value-eq 42 42))
  (should (t/eq:value-eq most-positive-fixnum most-positive-fixnum))
  ;; The same big integer (bignum) object, passed by reference..
  (let ((x (1+ most-positive-fixnum)))
    (should (t/eq:value-eq x x)))
  ;; An object passed through identity is eq to itself.
  (let ((x '(1 2 3)))
    (should (t/eq:value-eq x (t/identity x)))))

(ert-deftest eq::value-different-objects ()
  "Value == Value is false for distinct objects, even with equal contents."
  ;; Two big integers (bignums in Emacs 27+) are two separate objects. Without bignum support,
  ;; overflowing a fixnum just wraps around and stays a fixnum, which is always eq.
  (when t/support-bignum-p
    (should-not (t/eq:value-eq (1+ most-positive-fixnum) (1+ most-positive-fixnum))))
  ;; Two separately allocated strings with the same content are not eq.
  (should-not (t/eq:value-eq (t/eq:new-string "hello") (t/eq:new-string "hello")))
  ;; Two separately allocated conses are not eq.
  (should-not (t/eq:value-eq '(1) '(1)))
  ;; Different symbols are not eq.
  (should-not (t/eq:value-eq 'foo 'bar))
  ;; Different integers are not eq.
  (should-not (t/eq:value-eq 1 2)))

(ert-deftest eq::global-ref-same-object ()
  "GlobalRef == Value is true when they refer to the same underlying object."
  ;; t/eq:global-ref-eq makes a global ref from the first arg and compares it to the second.
  (should (t/eq:global-ref-eq 'hello 'hello))
  (let ((x '(1 2 3)))
    (should (t/eq:global-ref-eq x (t/identity x)))))

(ert-deftest eq::global-ref-different-objects ()
  "GlobalRef == Value is false for distinct objects."
  (should-not (t/eq:global-ref-eq 'foo 'bar))
  (should-not (t/eq:global-ref-eq '(1) '(1))))

(ert-deftest eq::classify-position ()
  "Dispatching on a Value by comparing it to use_symbols!-imported globals."
  (should (equal (t/eq:classify-position 'left)   "left"))
  (should (equal (t/eq:classify-position 'right)  "right"))
  (should (equal (t/eq:classify-position 'center) "center"))
  (should (equal (t/eq:classify-position 'other)  "unknown"))
  (should (equal (t/eq:classify-position nil)     "unknown")))
