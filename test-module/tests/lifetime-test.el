;;; -*- lexical-binding: t -*-

(require 't-helpers)

;;; ----------------------------------------------------------------------------
;;; Memory safety tests.

;;; Tests that, if failed, crash the whole process unrecoverably. They will be run under a
;;; sub-process Emacs.
(defmacro destructive-test (name &optional prefix)
  `(ert-deftest ,(intern (if prefix
                             (format "%s::%s" prefix name)
                           (format "%s" name))) ()
     (t/run-in-sub-process (intern ,(format "t/%s" name)))))

;;; TODO: The way this test is called is a bit convoluted.
(defun t/gc-after-catching ()
  (t/gc-after-catching-1
   (lambda () (error "abc"))))

(destructive-test gc-after-new-string lifetime)
(destructive-test gc-after-new-int lifetime)
(destructive-test gc-after-new-float lifetime)
(destructive-test gc-after-uninterning lifetime)
(destructive-test gc-after-retrieving lifetime)
(destructive-test gc-after-catching lifetime)

;;; ----------------------------------------------------------------------------
;;; Memory leak tests.

(defun t/free-global-ref-after-normal-return ()
  (t/trigger-double-free-global-ref
   (lambda ())))

(defun t/free-global-ref-after-error ()
  (t/trigger-double-free-global-ref
   (lambda () (error "This should not show up because Emacs should crash when run with --module-assertions"))))

(ert-deftest global-ref::free-after-normal-return ()
  (unless t/support-module-assertions-p
    (ert-skip "--module-assertions is not supported"))
  (when (bound-and-true-p module-rs-disable-gc-bug-31238-workaround)
    (ert-skip "Workaround for the GC bug 31238 was already disabled"))
  (should (string-match-p
           "Emacs value not found in"
           (cadr (t/get-error (t/run-in-sub-process 't/free-global-ref-after-normal-return))))))

(ert-deftest global-ref::free-after-error ()
  (unless t/support-module-assertions-p
    (ert-skip "--module-assertions is not supported"))
  (when (bound-and-true-p module-rs-disable-gc-bug-31238-workaround)
    (ert-skip "Workaround for the GC bug 31238 was already disabled"))
  (should (string-match-p
           "Emacs value not found in"
           (cadr (t/get-error (t/run-in-sub-process 't/free-global-ref-after-error))))))
