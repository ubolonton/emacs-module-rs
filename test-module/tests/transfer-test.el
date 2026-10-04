;;; -*- lexical-binding: t -*-
;;; user-ptr.

(require 't-helpers)

(ert-deftest transfer::vector ()

  (let* ((v1 (t/transfer-vector-make 5 6))
         (v2 (t/transfer-vector-make 1 3)))
    (should (string-prefix-p "#<user-ptr" (format "%s" v1)))
    (should (equal (t/transfer-vector-to-list v1) '(5 6)))
    (should (equal (t/transfer-vector-to-list
                    (t/transfer-vector-add v1 v2))
                   '(6 9)))
    ;; Emacs doesn't support custom equality...
    (should (not (equal (t/transfer-vector-add v1 v2)
                        (t/transfer-vector-add v1 v2))))
    ;; ... but should consider an object equal to itself.
    (should (let* ((v3 (t/transfer-vector-add v1 v2))
                   (v4 (t/identity v3)))
              (equal v3 v4))))

  ;; Mutation.
  (let ((v (t/transfer-vector-make 5 6)))
    (t/transfer-vector-scale-mutably 3 v)
    (should (equal (t/transfer-vector-to-list v) '(15 18)))
    (t/transfer-vector-swap-components v)
    (should (equal (t/transfer-vector-to-list v) '(18 15))))

  ;; ;; This is to trigger the finalizer. TODO: Somehow validate they actually runs.
  ;; (dotimes (i 100)
  ;;   (t/transfer-vector-make 1 2))
  ;; (garbage-collect)

  (let ((s (t/wrap-string "abc")))
    (should (string-prefix-p "#<user-ptr" (format "%s" s)))
    ;; TODO: Test 'rust-invalid-user-ptr. That probably requires 2 modules.
    (should-error (t/transfer-vector-to-list s)
                  :type 'rust-wrong-type-user-ptr)))

(ert-deftest transfer::ref-cell-borrow-conflict ()
  (let ((r (t/transfer-ref-cell-wrap 5)))
    (should (= (t/transfer-ref-cell-inc r) 6))
    (should (= (t/transfer-ref-cell-unwrap r) 6))
    (t/transfer-ref-cell-unwrap-and-call r (lambda () (t/transfer-ref-cell-unwrap r)))
    ;; FIX: Don't rely on error's string representation.
    (should (string-match-p "already borrowed"
                            (cadr (should-error (t/transfer-ref-cell-unwrap-and-call
                                                 r (lambda () (t/transfer-ref-cell-inc r)))
                                                :type 'rust-error))))))

(ert-deftest transfer::type-check ()
  (should-error (t/transfer-ref-cell-inc (t/transfer-vector-make 1 2))
                :type 'rust-wrong-type-user-ptr)
  (should-error (t/transfer-ref-cell-inc 5)
                :type 'wrong-type-argument)

  (let ((v (t/transfer-vector-make 1 2)))
    (should (equal (t/error:variant "ref-cell" v nil) "Rust/WrongTypeUserPtr"))
    ;; The data is (EXPECTED VALUE). EXPECTED is the Rust type name.
    (should (equal (should-error (t/transfer-ref-cell-inc v))
                   `(rust-wrong-type-user-ptr
                     ,(cadr (t/get-error (t/transfer-ref-cell-inc v)))
                     ,v)))
    (should (string-match-p "RefCell" (cadr (t/get-error (t/transfer-ref-cell-inc v)))))))

(ert-deftest transfer::hash-map ()
  (let ((m (t/transfer-hash-map-make)))
    (should (equal (t/transfer-hash-map-get m "a") nil))

    (should (equal (t/transfer-hash-map-set m "a" "1") nil))
    (should (equal (t/transfer-hash-map-get m "a") "1"))

    (should (equal (t/transfer-hash-map-set m "a" "2") "1"))
    (should (equal (t/transfer-hash-map-get m "a") "2"))))
