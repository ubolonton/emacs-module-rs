;;; user-ptr.

(require 't-helpers)

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
    (should (string-match-p "already borrowed"
                            (cadr (should-error (t/ref-cell-unwrap-and-call r (lambda () (t/ref-cell-inc r)))
                                                :type 'rust-error))))))

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
