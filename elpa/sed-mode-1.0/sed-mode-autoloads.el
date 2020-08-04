;;; sed-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sed-mode" "sed-mode.el" (0 0 0 0))
;;; Generated autoloads from sed-mode.el
 (add-to-list 'auto-mode-alist '("\\.sed\\'" . sed-mode))
 (add-to-list 'interpreter-mode-alist '("sed" . sed-mode))

(autoload 'sed-mode "sed-mode" "\
Sed editing mode.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sed-mode" '("sed-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sed-mode-autoloads.el ends here
