;;; germanium-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "germanium" "germanium.el" (0 0 0 0))
;;; Generated autoloads from germanium.el

(autoload 'germanium-install "germanium" "\
Install `germanium' via `go'." t nil)

(autoload 'germanium-region-to-png "germanium" "\
Generate a PNG file from current region between START and END.

\(fn START END)" t nil)

(autoload 'germanium-buffer-to-png "germanium" "\
Generate a PNG file from current buffer." t nil)

(register-definition-prefixes "germanium" '("germanium-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; germanium-autoloads.el ends here
