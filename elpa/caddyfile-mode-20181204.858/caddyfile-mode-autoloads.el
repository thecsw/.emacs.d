;;; caddyfile-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "caddyfile-mode" "caddyfile-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from caddyfile-mode.el

(autoload 'caddyfile-mode "caddyfile-mode" "\
Major mode for editing Caddy configuration files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("Caddyfile\\'" . caddyfile-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "caddyfile-mode" '("caddyfile-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; caddyfile-mode-autoloads.el ends here
