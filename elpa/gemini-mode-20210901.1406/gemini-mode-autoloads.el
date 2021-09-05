;;; gemini-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gemini-mode" "gemini-mode.el" (0 0 0 0))
;;; Generated autoloads from gemini-mode.el

(autoload 'gemini-mode "gemini-mode" "\
Major mode for editing text/gemini 'geminimap' documents

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.gmi\\'" . gemini-mode))

(add-to-list 'auto-mode-alist '("\\.gemini\\'" . gemini-mode))

(add-to-list 'auto-mode-alist '("\\.geminimap\\'" . gemini-mode))

(register-definition-prefixes "gemini-mode" '("gemini-" "turn-on-visual-fill-column-mode"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gemini-mode-autoloads.el ends here
