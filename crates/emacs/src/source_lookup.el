;;; -*- lexical-binding: t -*-

;; Lets `find-function', xref, and the `describe-function' link visit the Rust source of module
;; functions, using the locations that `#[defun]' records (see source.rs).
;;
;; Each module built with `#[module(source_lookup)]' evaluates this when it loads. So evaluating it
;; again must change nothing, and it must work with the properties that older crate versions set.
;;
;; These tools start with `symbol-file'. They reach `find-function-search-for-symbol' only if it
;; returns non-nil, i.e. if the module was loaded while a file was loading.

(defun emacs-module-rs--find-source (symbol)
  "Visit the Rust source of the module function SYMBOL.
Return (BUFFER . POSITION).  Return nil if SYMBOL has no recorded
location, its module's feature has no `emacs-module-rs-source-root',
or the file does not exist."
  (pcase (get symbol 'emacs-module-rs-source)
    (`(,file ,line ,feature)
     (let ((root (get feature 'emacs-module-rs-source-root)))
       (when (stringp root)
         (let ((path (expand-file-name file root)))
           (when (file-readable-p path)
             (with-current-buffer (find-file-noselect path)
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (cons (current-buffer) (point)))))))))))

(defun emacs-module-rs--find-function-search-for-symbol (find-fn symbol type library)
  "Around advice for `find-function-search-for-symbol'.
If SYMBOL is a module function with a known Rust source, visit it and
ignore LIBRARY, which is the module file, or the file that loaded it.
Otherwise call FIND-FN with SYMBOL, TYPE, and LIBRARY."
  (or (and (null type)
           (symbolp symbol)
           (emacs-module-rs--find-source symbol))
      (funcall find-fn symbol type library)))

(advice-add 'find-function-search-for-symbol :around
            #'emacs-module-rs--find-function-search-for-symbol)
