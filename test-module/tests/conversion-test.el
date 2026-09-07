;;; -*- lexical-binding: t -*-
;;; Type conversion.

(require 't-helpers)

(ert-deftest conversion::integers ()
  (should (= (t/inc 3) 4))
  (should (string-match-p (regexp-quote "1+")
                          (documentation 't/inc) ))

  (should-error (t/inc "3") :type 'wrong-type-argument)
  (should-error (t/inc nil) :type 'wrong-type-argument)

  (should-error (t/inc) :type 'wrong-number-of-arguments)
  (should-error (t/inc 1 2) :type 'wrong-number-of-arguments)

  (should (= -128 (t/conversion-identity-i8 -128)))
  (should (= 255 (t/conversion-identity-u8 255)))

  ;; FIX: Don't rely on error's string representation.
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/conversion-u64-overflow) :type 'rust-error))))
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/conversion-identity-i8 128) :type 'rust-error))))
  (should (string-match-p
           "out of range"
           (cadr (should-error (t/conversion-identity-u8 -1) :type 'rust-error)))))

(ert-deftest conversion::passthrough ()
  (let ((x "x"))
    (should (eq (t/identity x) x))
    (should (eq (t/identity 5) 5))
    (should (string-match-p (regexp-quote "Return the input (not a copy).")
                            (documentation #'t/identity) ))))

(ert-deftest conversion::string-basic ()
  (should (equal (t/to-uppercase "abc") "ABC"))
  ;; copy_string_contents copies the null terminator.
  (should-error (t/conversion-copy-string-contents "xyz" 3) :type t/buffer-too-small-error-type)
  (should-error (t/conversion-copy-string-contents "" 0) :type t/buffer-too-small-error-type)
  (should (string= "xyz" (t/conversion-copy-string-contents "xyz" 4)))
  (should (string= "" (t/conversion-copy-string-contents "" 1)))
  (should-error (t/conversion-copy-string-contents "abcxyz" 3) :type t/buffer-too-small-error-type))

(ert-deftest conversion::string-coding ()
  (should (equal (t/conversion-string-to-bytes
                  "a")
                 [97]))
  ;; 2-byte UTF-8.
  (should (equal (t/conversion-string-to-bytes
                  "á")
                 [195 161]))
  ;; Trailing zero byte.
  (should (equal (t/conversion-string-to-bytes
                  (unibyte-string 97 0))
                 [97 0]))
  ;; Zero byte in the middle.
  (should (equal (t/conversion-string-to-bytes
                  (string-to-multibyte
                   (unibyte-string 97 0 98)))
                 [97 0 98]))
  (should (equal (t/conversion-string-to-bytes
                  (encode-coding-string
                   (concat (unibyte-string #x97) "π")
                   'utf-8))
                 [151 207 128]))
  (should-error (t/conversion-string-to-bytes
                 (concat (unibyte-string #x97) "π"))
                :type 'wrong-type-argument))

(ert-deftest conversion::option-string ()
  (should (equal (t/conversion-to-lowercase-or-nil "CDE") "cde"))
  (should (equal (t/conversion-to-lowercase-or-nil nil) nil))
  (should-error (t/conversion-to-lowercase-or-nil 1) :type 'wrong-type-argument))

(ert-deftest conversion::vector-functions ()
  (should (equal (t/conversion-make-vector 5 nil) (make-vector 5 nil)))
  (let ((v [0 1 2 3]))
    (should (= 4 (t/conversion-vec-size v)))
    (t/conversion-vec-set v 2 'a)
    (should (eq 'a (t/conversion-vec-get v 2)))
    (should-error (t/conversion-vec-get v -1) :type 'args-out-of-range)
    (should-error (t/conversion-vec-set v -5 'a) :type 'args-out-of-range))
  (let ((v [a b c d e]))
    (should (eq v (t/conversion-identity-if-vector v)))
    (should-error (t/conversion-identity-if-vector nil) :type 'wrong-type-argument)
    (should (equal (t/get-error (eq "abc" (t/conversion-identity-if-vector "abc")))
                   '(wrong-type-argument vectorp "abc"))))
  (let ((v [0 1 2 3]))
    (should (eq v (t/conversion-stringify-num-vector v)))
    (should (equal v ["0" "1" "2" "3"]))
    (should-error (t/conversion-stringify-num-vector v) :type 'wrong-type-argument)))
