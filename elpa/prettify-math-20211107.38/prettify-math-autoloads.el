;;; prettify-math-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prettify-math" "prettify-math.el" (0 0 0 0))
;;; Generated autoloads from prettify-math.el

(autoload 'prettify-math-mode "prettify-math" "\
prettify math mode base on font lock

This is a minor mode.  If called interactively, toggle the
`Prettify-Math mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `prettify-math-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-prettify-math-mode 'globalized-minor-mode t)

(defvar global-prettify-math-mode nil "\
Non-nil if Global Prettify-Math mode is enabled.
See the `global-prettify-math-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-prettify-math-mode'.")

(custom-autoload 'global-prettify-math-mode "prettify-math" nil)

(autoload 'global-prettify-math-mode "prettify-math" "\
Toggle Prettify-Math mode in all buffers.
With prefix ARG, enable Global Prettify-Math mode if ARG is
positive; otherwise, disable it.  If called from Lisp, enable the mode if ARG
is omitted or nil.

Prettify-Math mode is enabled in all buffers where
`prettify-math-mode' would do it.

See `prettify-math-mode' for more information on Prettify-Math mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "prettify-math" '("prettify-math-"))

;;;***

;;;### (autoloads nil nil ("prettify-math-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prettify-math-autoloads.el ends here
