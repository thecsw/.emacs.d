;;; mips-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mips-mode" "mips-mode.el" (0 0 0 0))
;;; Generated autoloads from mips-mode.el

(autoload 'mips-mode "mips-mode" "\
Major mode for editing MIPS assembler code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.mips\\'" . mips-mode))

(register-definition-prefixes "mips-mode" '("mips-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mips-mode-autoloads.el ends here
