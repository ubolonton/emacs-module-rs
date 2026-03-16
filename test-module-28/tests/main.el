(require 't28)

;;; ----------------------------------------------------------------------------
;;; open-channel tests (Emacs 28+).

(ert-deftest channel::send-and-receive ()
  (skip-unless (>= emacs-major-version 28))
  (let* ((received nil)
         (proc (make-pipe-process
                :name "test-send"
                :filter (lambda (_proc output)
                          (setq received (concat received output))))))
    (unwind-protect
        (progn
          (t28/channel-send proc "hello from rust")
          (accept-process-output proc 5)
          (should (equal received "hello from rust")))
      (delete-process proc))))

(ert-deftest channel::send-from-thread ()
  (skip-unless (>= emacs-major-version 28))
  (let* ((received nil)
         (proc (make-pipe-process
                :name "test-thread-send"
                :filter (lambda (_proc output)
                          (setq received (concat received output))))))
    (unwind-protect
        (progn
          (t28/channel-send-from-thread proc "threaded hello")
          (accept-process-output proc 5)
          (should (equal received "threaded hello")))
      (delete-process proc))))

(ert-deftest channel::wrong-type-arg ()
  (skip-unless (>= emacs-major-version 28))
  (should-error (t28/channel-send "not-a-process" "data")
                :type 'wrong-type-argument))
