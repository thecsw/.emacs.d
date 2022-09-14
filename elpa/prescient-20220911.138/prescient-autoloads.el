;;; prescient-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prescient" "prescient.el" (0 0 0 0))
;;; Generated autoloads from prescient.el

(autoload 'prescient-filter "prescient" "\
Use QUERY to filter list of CANDIDATES.

CANDIDATES is a completion table, such as a list of strings
or a function as defined in the Info node
`(elisp)Programmed Completion'.

QUERY is a string containing the sub-queries, which are gotten
using `prescient-split-query'. Each sub-query is used to produce
a regular expression according to the filter methods listed in
`prescient-filter-method'. A candidate must match every regular
expression made from the sub-queries to be included in the list
of returned candidates.

PRED is the predicate used with the completion table, as
described in the above Info node.

This function does not modify CANDIDATES; it always make a new
copy of the list.

\(fn QUERY CANDIDATES &optional PRED)" nil nil)

(autoload 'prescient-completion-sort "prescient" "\
Sort the filtered CANDIDATES.

This function is a wrapper around `prescient-sort' and the
function `prescient-sort-full-matches-first'. It is designed for
use with the `prescient' completion style, though it might also
be useful in other cases.

In Emacs 27 and later, when filtering via the `prescient'
completion style, if `prescient-completion-enable-sort' is
non-nil, then completion tables that do not specify a sorting
function are modified to use this function. This does not effect
the sorting done by other completion styles or by completion UIs
when those styles are filtering.

Therefore, if you want prescient.el sorting to be used with other
completion styles, consider setting your UI of choice to use this
function when no other function is given. If you explicitly set
the sorting function to this function or `prescient-sort', then
be sure that `prescient-completion-enable-sort' is nil (the
default) to avoid mistakenly sorting the candidates twice.

This function checks for the properties `prescient-regexps' and
`prescient-ignore-case' on the first candidate in
CANDIDATES (though they are stored on all candidates filtered by
the `prescient' style). These properties are set during
`prescient-filter', and are used for implementing the user option
`prescient-sort-full-matches-first'.

\(fn CANDIDATES)" nil nil)

(autoload 'prescient-all-completions "prescient" "\
`all-completions' using prescient.el.

STRING is the input. TABLE is a completion table. PRED is a
predicate that further restricts the matching candidates. POINT
would be the current point, but it is not used by this function.
See the function `all-completions' for more information.

This function returns a list of completions whose final `cdr' is
the length of the prefix string used for completion (which might
be all or just part of STRING).

\(fn STRING TABLE &optional PRED POINT)" nil nil)

(autoload 'prescient-try-completion "prescient" "\
`try-completion' using Prescient.

STRING is the input.  TABLE is a completion table.  PRED is a
predicate.  POINT is the current point.  See the function
`try-completion' for more information.

If there are no matches, this function returns nil. If the only
match equals STRING, this function returns t. Otherwise, this
function returns a cons cell of the completed string and its
length. If there is more than one match, that completed string is
actually just the input, in which case nothing happens.

\(fn STRING TABLE &optional PRED POINT)" nil nil)

(add-to-list 'completion-styles-alist '(prescient prescient-try-completion prescient-all-completions "Filtering (and optionally sorting) using prescient.el.\nTo enable sorting done by the completion style itself, see\n`prescient-completion-enable-sort', which requires Emacs 27 or\nlater. Otherwise, see the function `prescient-completion-sort'."))

(put 'prescient 'completion--adjust-metadata 'prescient--completion-modify-sort)

(register-definition-prefixes "prescient" '("prescient-" "selectrum-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prescient-autoloads.el ends here
