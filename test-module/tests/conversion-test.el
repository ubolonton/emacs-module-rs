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

(ert-deftest conversion::string-unicode-to-bytes ()
  (dolist (multibyte-unicode-str
           (list "testing"
                 "π là số vô tỉ"
                 "
á -> 2 bytes
€ -> 3 bytes
😊 -> 4 bytes"
                 ;; With null bytes in the middle and trailing.
                 (string ?π 0 0 ?á 0 0)))
    (let* ((unibyte-str (string-as-unibyte multibyte-unicode-str))
           (bytes (vconcat unibyte-str)))
      ;; Both multibyte and unibyte representations should work.
      (should (equal (t/conversion-string-to-bytes multibyte-unicode-str)
                     bytes))
      (should (equal (t/conversion-string-to-bytes unibyte-str)
                     bytes)))))

(ert-deftest conversion::string-non-unicode-to-bytes ()
  (dolist (multibyte-non-unicode-str
           (list (string #x3FFFFF       ;  raw byte FF
                         ;; With null bytes in the middle and trailing.
                         ?π 0 0 ?á 0 0)
                 (decode-coding-string "\x96\xa4\xa2\xa4" 'emacs-mule)
                 (decode-coding-string
                  (encode-coding-string "Nguyễn Tuấn Anh" 'vietnamese-vscii) 'utf-8-emacs)
                 (decode-coding-string
                  (encode-coding-string "阮俊英" 'chinese-big5) 'utf-8-emacs)))
    (let ((err (should-error
                (t/conversion-string-to-bytes multibyte-non-unicode-str)
                :type 'wrong-type-argument))
          (unibyte-str (string-as-unibyte multibyte-non-unicode-str)))
      ;; Multibyte representation is rejected.
      (should (eq (cadr err) 'unicode-string-p))
      ;; Unibyte representation is accepted.
      (should (equal (t/conversion-string-to-bytes unibyte-str)
                     (vconcat unibyte-str))))))

(ert-deftest conversion::string-unicode-roundtrip ()
  (dolist (multibyte-unicode-str
           (list "testing"
                 "π là số vô tỉ"
                 "
á -> 2 bytes
€ -> 3 bytes
😊 -> 4 bytes"
                 ;; With null bytes in the middle and trailing.
                 (string ?π 0 0 ?á 0 0)))
    (let ((unibyte-str (string-as-unibyte multibyte-unicode-str)))
      ;; Both multibyte and unibyte representations should work.
      (should (equal (t/conversion-string-roundtrip multibyte-unicode-str)
                     multibyte-unicode-str))
      (should (equal (t/conversion-string-roundtrip unibyte-str)
                     multibyte-unicode-str)))))

(ert-deftest conversion::string-non-unicode-roundtrip ()
  (dolist (multibyte-non-unicode-str
           (list (string #x3FFFFF       ;  raw byte FF
                         ;; With null bytes in the middle and trailing.
                         ?π 0 0 ?á 0 0)
                 (decode-coding-string "\x96\xa4\xa2\xa4" 'emacs-mule)
                 (decode-coding-string
                  (encode-coding-string "Nguyễn Tuấn Anh" 'vietnamese-vscii) 'utf-8-emacs)
                 (decode-coding-string
                  (encode-coding-string "阮俊英" 'chinese-big5) 'utf-8-emacs)))
    (let ((err (should-error
                (t/conversion-string-roundtrip multibyte-non-unicode-str)
                :type 'wrong-type-argument))
          (unibyte-str (string-as-unibyte multibyte-non-unicode-str)))
      ;; Multibyte representation is rejected on the Emacs side.
      (should (eq (cadr err) 'unicode-string-p))
      ;; Unibyte representation is rejected on the Rust side.
      (should (string-match-p
               "invalid utf-8 sequence"
               (cadr (should-error (t/conversion-string-roundtrip unibyte-str)
                                   :type 'rust-error)))))))

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
