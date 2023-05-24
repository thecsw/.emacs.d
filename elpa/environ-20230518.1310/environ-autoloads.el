;;; environ-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "environ" "environ.el" (0 0 0 0))
;;; Generated autoloads from environ.el

(autoload 'environ-set-file "environ" "\
Set environment variables defined in the file at FILE-PATH.
When used interactively, prompts for the file to load. The prompt begins in
`environ-dir'. When used from elisp, FILE-PATH can either be absolute or
relative to `default-directory'.

\(fn FILE-PATH)" t nil)

(autoload 'environ-unset-file "environ" "\
Unset the environment variables defined in FILE-PATH.
See the documentation for `environ-set-file'.

\(fn FILE-PATH)" t nil)

(autoload 'environ-unset-name "environ" "\
Unset the environment variable NAME.
Unset the given environment variable by removing it from
`process-environment' if it is there. Note that calling `setenv' with a
prefix argument can unset a variable by setting its value to nil, but the
variable remains in `process-environment'. This function completely removes
the variable from `process-environment'.

\(fn NAME)" t nil)

(register-definition-prefixes "environ" '("environ-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; environ-autoloads.el ends here
