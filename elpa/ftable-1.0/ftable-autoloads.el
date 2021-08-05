;;; ftable-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ftable" "ftable.el" (0 0 0 0))
;;; Generated autoloads from ftable.el

(autoload 'ftable-fill "ftable" "\
Fill the table (or paragraph) at point." t nil)

(autoload 'ftable-edit-cell "ftable" "\
Edit the cell at point." t nil)

(autoload 'ftable-reformat "ftable" "\
Change box drawing STYLE for table at point.
STYLE can be ’ascii or ’unicode.

\(fn STYLE)" t nil)

(register-definition-prefixes "ftable" '("ftable-"))

;;;***

;;;### (autoloads nil nil ("ftable-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ftable-autoloads.el ends here
