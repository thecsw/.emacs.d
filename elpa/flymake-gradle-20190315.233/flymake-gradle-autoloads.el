;;; flymake-gradle-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake-gradle" "flymake-gradle.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from flymake-gradle.el

(autoload 'flymake-gradle-setup "flymake-gradle" "\
Set up Flymake for Gradle." t nil)

(autoload 'flymake-gradle-add-hook "flymake-gradle" "\
Add `flymake-gradle-lint' to `flymake-diagnostic-functions'." nil nil)

(register-definition-prefixes "flymake-gradle" '("flymake-gradle-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-gradle-autoloads.el ends here
