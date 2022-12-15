;;; boxes-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "boxes" "boxes.el" (0 0 0 0))
;;; Generated autoloads from boxes.el

(autoload 'boxes-create "boxes" "\
Automagicly create a new box around the region based on the default type." t nil)

(autoload 'boxes-remove "boxes" "\
Automagicly remove a new box around the region based on the default type." t nil)

(autoload 'boxes-command-on-region "boxes" "\
Create or Remove boxes from a region.

To create a box select a region, hit \\[boxes-command-on-region]
& enter a box type.  Box type selection uses tab completion on
the supported types.

To remove a box simply prefix a 1 to the call, eg M-1
\\[boxes-command-on-region] will remove a box from a region.

Note that interactive use requires `boxes' >= 2.1.0 to support
querying the supported types.

When calling from Lisp, supply the region START & END and the box
TYPE to create a box.  Specifying a non-nil value for REMOVE,
removes the box.

\(fn START END TYPE &optional REMOVE)" t nil)

(register-definition-prefixes "boxes" '("boxes-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; boxes-autoloads.el ends here
