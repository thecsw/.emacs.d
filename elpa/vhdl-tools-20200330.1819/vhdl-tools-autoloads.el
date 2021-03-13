;;; vhdl-tools-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vhdl-tools" "vhdl-tools.el" (0 0 0 0))
;;; Generated autoloads from vhdl-tools.el

(autoload 'vhdl-tools-mode "vhdl-tools" "\
Utilities for navigating vhdl sources.

If called interactively, toggle `Vhdl-Tools mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Key bindings:
\\{map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "vhdl-tools" '("vhdl-tools-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vhdl-tools-autoloads.el ends here
