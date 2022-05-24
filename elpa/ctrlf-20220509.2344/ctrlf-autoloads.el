;;; ctrlf-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ctrlf" "ctrlf.el" (0 0 0 0))
;;; Generated autoloads from ctrlf.el

(defvar ctrlf-mode-map (let ((keymap (make-sparse-keymap))) (define-key keymap [remap isearch-forward] #'ctrlf-forward-default) (define-key keymap [remap isearch-backward] #'ctrlf-backward-default) (define-key keymap [remap isearch-forward-regexp] #'ctrlf-forward-alternate) (define-key keymap [remap isearch-backward-regexp] #'ctrlf-backward-alternate) (define-key keymap [remap isearch-forward-symbol] #'ctrlf-forward-symbol) (define-key keymap [remap isearch-forward-symbol-at-point] #'ctrlf-forward-symbol-at-point) keymap) "\
Keymap used by CTRLF globally.")

(custom-autoload 'ctrlf-mode-map "ctrlf" t)

(defvar ctrlf-mode-bindings '(([remap isearch-forward] . ctrlf-forward-default) ([remap isearch-backward] . ctrlf-backward-default) ([remap isearch-forward-regexp] . ctrlf-forward-alternate) ([remap isearch-backward-regexp] . ctrlf-backward-alternate) ([remap isearch-forward-symbol] . ctrlf-forward-symbol) ([remap isearch-forward-symbol-at-point] . ctrlf-forward-symbol-at-point)) "\
This variable is deprecated.
To customize the keybindings, modify `ctrlf-mode-map' directly.

Keybindings enabled in `ctrlf-mode'. This is not a keymap.
Rather it is an alist that is converted into a keymap just before
`ctrlf-mode' is (re-)enabled. The keys are strings or raw key
events and the values are command symbols.

These bindings are available globally in Emacs. See also
`ctrlf-minibuffer-bindings', which defines bindings that are
active in the minibuffer during a search.")

(custom-autoload 'ctrlf-mode-bindings "ctrlf" nil)

(autoload 'ctrlf-forward-default "ctrlf" "\
Search forward using the default search style.
The default search style is specified in
`ctrlf-default-search-style'. If already in a search, go to next
candidate, or if no input then insert the previous search string.
If in a different search than `ctrlf-default-search-style',
change back to that style if prefix ARG is provided. If in the
minibuffer but not in a search already, run fallback isearch
function instead.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-backward-default "ctrlf" "\
Search backward using the default search style.
The default search style is specified in
`ctrlf-default-search-style'. If already in a search, go to
previous candidate, or if no input then insert the previous
search string. If in a different search than
`ctrlf-default-search-style', change back to that style if prefix
ARG is provided. If in the minibuffer but not in a search
already, run fallback isearch function instead.

\(fn &optional ARG)" t nil)

(autoload 'ctrlf-forward-alternate "ctrlf" "\
Search forward using the alternate search style.
The default search style is specified in
`ctrlf-alternate-search-style'. If already in a search, go to
next candidate, or if no input then insert the previous search
string. If in a different search than
`ctrlf-alternate-search-style', change back to that style. If in
the minibuffer but not in a search already, run fallback isearch
function instead." t nil)

(autoload 'ctrlf-backward-alternate "ctrlf" "\
Search backward using the alternate search style.
The default search style is specified in
`ctrlf-alternate-search-style'. If already in a search, go to
previous candidate, or if no input then insert the previous
search string. If in a different search than
`ctrlf-alternate-search-style', change back to that style. If in
the minibuffer but not in a search already, run fallback isearch
function instead." t nil)

(autoload 'ctrlf-forward-literal "ctrlf" "\
Search forward for literal string.
If already in a search, go to next candidate, or if no input then
insert the previous search string. If in a non-literal search,
change back to regexp search. If in the minibuffer but not in a
search already, run the function `isearch-forward' instead." t nil)

(autoload 'ctrlf-backward-literal "ctrlf" "\
Search backward for literal string.
If already in a search, go to previous candidate, or if no input
then insert the previous search string. If in a non-literal
search, change back to literal search. If in the minibuffer but
not in a search already, run `isearch-backward' instead." t nil)

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

(define-minor-mode ctrlf-local-mode "\
Minor mode to use CTRLF in place of Isearch." :keymap ctrlf-mode-map (require 'map) (let ((default-ctrlf-mode-bindings (eval (car (get 'ctrlf-mode-bindings 'standard-value))))) (when (and ctrlf-local-mode default-ctrlf-mode-bindings (not (equal ctrlf-mode-bindings default-ctrlf-mode-bindings))) (when ctrlf--ctrlf-mode-bindings-deprecation-warning (message "Variable `ctrlf-mode-bindings' is deprecated. Please use `ctrlf-mode-map' to customize your keybindings instead.") (setq ctrlf--ctrlf-mode-bindings-deprecation-warning nil)) (setcdr ctrlf-mode-map nil) (map-apply (lambda (key cmd) (when (stringp key) (setq key (kbd key))) (define-key ctrlf-mode-map key cmd)) ctrlf-mode-bindings))) (with-eval-after-load 'ctrlf (if ctrlf-local-mode (advice-add #'minibuffer-message :around #'ctrlf--minibuffer-message-condense) (advice-remove #'minibuffer-message #'ctrlf--minibuffer-message-condense))))

(defvar ctrlf--fake-mode-map (make-sparse-keymap) "\
Empty keymap used to hack around an Emacs limitation.
See https://github.com/radian-software/ctrlf/issues/103.
Apparently, when you define a globalized minor mode, it forces
you to associate a keymap with it, and for CTRLF the default name
of this keymap is actually the keymap we are using for the local
map, which has the consequence of the local keymap taking effect
globally. The only workaround I could think of was to point the
global mode at a fake empty keymap.")

(define-globalized-minor-mode ctrlf-mode ctrlf-local-mode ctrlf-local-mode :keymap ctrlf--fake-mode-map)

(register-definition-prefixes "ctrlf" '("ctrlf-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ctrlf-autoloads.el ends here
