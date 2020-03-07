;;; lsp-scala-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-scala" "lsp-scala.el" (0 0 0 0))
;;; Generated autoloads from lsp-scala.el

(defvar lsp-scala-server-command "metals-emacs" "\
The command to launch the Scala language server.")

(custom-autoload 'lsp-scala-server-command "lsp-scala" t)

(defvar lsp-scala-server-args 'nil "\
Extra arguments for the Scala language server.")

(custom-autoload 'lsp-scala-server-args "lsp-scala" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-scala" '("lsp-scala-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-scala-autoloads.el ends here
