;;; emms-state-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emms-state" "emms-state.el" (0 0 0 0))
;;; Generated autoloads from emms-state.el

(defvar emms-state-mode nil "\
Non-nil if Emms-State mode is enabled.
See the `emms-state-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `emms-state-mode'.")

(custom-autoload 'emms-state-mode "emms-state" nil)

(autoload 'emms-state-mode "emms-state" "\
Minor mode for displaying some EMMS info in the mode line.

If called interactively, enable Emms-State mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This mode is intended to be a substitution for `emms-mode-line'
and `emms-playing-time'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-state" '("emms-state")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emms-state-autoloads.el ends here
