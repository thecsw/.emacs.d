;;; fsharp-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from fsharp-mode.el

(add-to-list 'auto-mode-alist '("\\.fs[iylx]?\\'" . fsharp-mode))
(autoload 'fsharp-mode "fsharp-mode" "\


(fn)" t)
(register-definition-prefixes "fsharp-mode" '("fsharp-" "running-xemacs"))


;;; Generated autoloads from fsharp-mode-font.el

(register-definition-prefixes "fsharp-mode-font" '("def-fsharp-compiled-var" "fsharp-"))


;;; Generated autoloads from fsharp-mode-structure.el

(register-definition-prefixes "fsharp-mode-structure" '("beginning-of-fsharp-def-or-class" "fsharp-"))


;;; Generated autoloads from fsharp-mode-util.el

(register-definition-prefixes "fsharp-mode-util" '("fsharp-"))


;;; Generated autoloads from inf-fsharp-mode.el

(autoload 'run-fsharp "inf-fsharp-mode" "\
Run an inferior fsharp process.
Input and output via buffer `*inferior-fsharp*'.

(fn &optional CMD)" t)
(register-definition-prefixes "inf-fsharp-mode" '("fsharp-" "inferior-fsharp-"))

;;; End of scraped data

(provide 'fsharp-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; fsharp-mode-autoloads.el ends here