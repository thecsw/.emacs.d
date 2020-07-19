;;; undo-propose-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "undo-propose" "undo-propose.el" (0 0 0 0))
;;; Generated autoloads from undo-propose.el

(autoload 'undo-propose "undo-propose" "\
Navigate undo history in a new temporary buffer.
\\<undo-propose-mode-map>
Copies 'current-buffer' and 'buffer-undo-list' to a new temporary buffer,
which is read-only except for undo commands.  After finished undoing, type
\\[undo-propose-commit] to accept the chain of undos,
or \\[undo-propose-squash-commit] to copy the buffer but squash the undo's into a single edit event event.  To cancel, type \\[undo-propose-cancel], and
to view an ediff type \\[undo-propose-diff].

If already inside an `undo-propose' buffer, this will simply call `undo'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-propose" '("undo-propose-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; undo-propose-autoloads.el ends here
