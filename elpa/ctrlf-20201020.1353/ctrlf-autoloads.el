;;; ctrlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ctrlf" "ctrlf.el" (0 0 0 0))
;;; Generated autoloads from ctrlf.el

(defvar ctrlf-mode-bindings '(("C-s" . ctrlf-forward-literal) ("C-r" . ctrlf-backward-literal) ("C-M-s" . ctrlf-forward-regexp) ("C-M-r" . ctrlf-backward-regexp) ("M-s _" . ctrlf-forward-symbol) ("M-s ." . ctrlf-forward-symbol-at-point)) "\
Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols.

These bindings are available globally in Emacs. See also
`ctrlf-minibuffer-bindings', which defines bindings that are
active in the minibuffer during a search.")

(custom-autoload 'ctrlf-mode-bindings "ctrlf" nil)

(autoload 'ctrlf-forward-literal "ctrlf" "\
Search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-literal search,
change back to literal search if prefix ARG is provided. If in
the minibuffer but not in a search already, run command
`isearch-forward' instead.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-backward-literal "ctrlf" "\
Search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-literal
search, change back to literal search if prefix ARG is provided.
If in the minibuffer but not in a search already, run
`isearch-backward' instead.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-forward-regexp "ctrlf" "\
Search forward for regexp.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-regexp search,
change back to regexp search." t nil)

(autoload 'ctrlf-backward-regexp "ctrlf" "\
Search backward for regexp.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-regexp
search, change back to regexp search." t nil)

(autoload 'ctrlf-forward-symbol "ctrlf" "\
Search forward for symbol.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-symbol search,
change back to symbol search." t nil)

(autoload 'ctrlf-forward-symbol-at-point "ctrlf" "\
Search forward for symbol at point.
If already in a search, replace the current input and change to a
symbol search, otherwise start the search. If no symbol is found,
display an error message and do not search." t nil)

(autoload 'ctrlf-occur "ctrlf" "\
Run `occur' using the last search string as the regexp." t nil)

(autoload 'ctrlf-forward-fuzzy "ctrlf" "\
Fuzzy search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-fuzzy search,
change back to fuzzy search." t nil)

(autoload 'ctrlf-backward-fuzzy "ctrlf" "\
Fuzzy search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-fuzzy search,
change back to fuzzy search." t nil)

(autoload 'ctrlf-forward-fuzzy-regexp "ctrlf" "\
Fuzzy search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-fuzzy-regexp
search, change back to fuzzy-regexp search." t nil)

(autoload 'ctrlf-backward-fuzzy-regexp "ctrlf" "\
Fuzzy search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-fuzzy-regexp
search, change back to fuzzy-regexp search." t nil)

(defvar ctrlf--keymap (make-sparse-keymap) "\
Keymap for `ctrlf-mode'. Populated when mode is enabled.
See `ctrlf-mode-bindings'.")

(define-minor-mode ctrlf-local-mode "\
Minor mode to use CTRLF in place of Isearch.
See `ctrlf-mode-bindings' to customize." :keymap ctrlf--keymap (require 'map) (when ctrlf-local-mode (setcdr ctrlf--keymap nil) (map-apply (lambda (key cmd) (when (stringp key) (setq key (kbd key))) (define-key ctrlf--keymap key cmd)) ctrlf-mode-bindings)) (with-eval-after-load 'ctrlf (if ctrlf-local-mode (advice-add #'minibuffer-message :around #'ctrlf--minibuffer-message-condense) (advice-remove #'minibuffer-message #'ctrlf--minibuffer-message-condense))))

(define-globalized-minor-mode ctrlf-mode ctrlf-local-mode ctrlf-local-mode)

(register-definition-prefixes "ctrlf" '("ctrlf-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ctrlf-autoloads.el ends here
