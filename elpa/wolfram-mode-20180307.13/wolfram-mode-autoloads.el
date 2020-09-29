;;; wolfram-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "wolfram-mode" "wolfram-mode.el" (0 0 0 0))
;;; Generated autoloads from wolfram-mode.el

(autoload 'wolfram-mode "wolfram-mode" "\
Major mode for editing Mathematica text files in Emacs.

\\{wolfram-mode-map}
Entry to this mode calls the value of `wolfram-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(autoload 'run-wolfram "wolfram-mode" "\
Run an inferior Mathematica process CMD, input and output via buffer *wolfram*.

\(fn CMD)" t nil)

(register-definition-prefixes "wolfram-mode" '("inferior-wolfram-mode" "wolfram-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wolfram-mode-autoloads.el ends here
