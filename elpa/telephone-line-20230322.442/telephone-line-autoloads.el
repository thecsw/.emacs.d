;;; telephone-line-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "telephone-line" "telephone-line.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telephone-line.el

(defvar telephone-line-mode nil "\
Non-nil if Telephone-Line mode is enabled.
See the `telephone-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telephone-line-mode'.")

(custom-autoload 'telephone-line-mode "telephone-line" nil)

(autoload 'telephone-line-mode "telephone-line" "\
Toggle telephone-line on or off.

This is a minor mode.  If called interactively, toggle the
`Telephone-Line mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='telephone-line-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "telephone-line" '("telephone-line-"))

;;;***

;;;### (autoloads nil "telephone-line-config" "telephone-line-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-config.el

(autoload 'telephone-line-evil-config "telephone-line-config" "\
Deprecated, just call (telephone-line-mode t) instead." nil nil)

;;;***

;;;### (autoloads nil "telephone-line-segments" "telephone-line-segments.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-segments.el

(register-definition-prefixes "telephone-line-segments" '("telephone-line-"))

;;;***

;;;### (autoloads nil "telephone-line-separators" "telephone-line-separators.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-separators.el

(register-definition-prefixes "telephone-line-separators" '("telephone-line-"))

;;;***

;;;### (autoloads nil "telephone-line-utils" "telephone-line-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telephone-line-utils.el

(autoload 'telephone-line-defsegment* "telephone-line-utils" "\
Define NAME as a segment function.

Does not check if segment is empty; will always display on non-nil result.

\(fn NAME &rest BODY)" nil t)

(function-put 'telephone-line-defsegment* 'doc-string-elt '3)

(function-put 'telephone-line-defsegment* 'lisp-indent-function 'defun)

(autoload 'telephone-line-defsegment "telephone-line-utils" "\
Define NAME as a segment function.

Empty strings will not render.

\(fn NAME &rest BODY)" nil t)

(function-put 'telephone-line-defsegment 'doc-string-elt '3)

(function-put 'telephone-line-defsegment 'lisp-indent-function 'defun)

(autoload 'telephone-line-raw "telephone-line-utils" "\
Conditionally render STR as mode-line data.
If optional argument PREFORMATTED is non-nil, verify the output
instead.
Return nil for blank/empty strings.

\(fn STR &optional PREFORMATTED)" nil nil)

(register-definition-prefixes "telephone-line-utils" '("telephone-line-"))

;;;***

;;;### (autoloads nil nil ("telephone-line-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; telephone-line-autoloads.el ends here
