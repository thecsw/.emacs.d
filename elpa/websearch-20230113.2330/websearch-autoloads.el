;;; websearch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "websearch" "websearch.el" (0 0 0 0))
;;; Generated autoloads from websearch.el

(autoload 'websearch-browse-with "websearch" "\
Set the function used to browse full query URLs to BROWSE-URL-FUNCTION.

\(fn BROWSE-URL-FUNCTION)" t nil)

(autoload 'websearch-point "websearch" "\
Query search engines based on `thing-at-point'." t nil)

(autoload 'websearch-region "websearch" "\
Query search engines based on selected buffer region.

START and END come from the selected region, they form the search term.

\(fn START END)" t nil)

(autoload 'websearch-kill-ring "websearch" "\
Query search engines based on ‘kill-ring’." t nil)

(autoload 'websearch-term "websearch" "\
Query search engines based on SEARCH-TERM input from prompt.

\(fn SEARCH-TERM)" t nil)

(autoload 'websearch "websearch" "\
Query search engines with a METHOD-NAME.

The list of possible selections is defined by ‘websearch-methods’.

\(fn METHOD-NAME)" t nil)

(autoload 'websearch-define "websearch" "\
Define a dwim function to search the web using `websearch'.
Unless called with FUNCTION as nil, then only add to `websearch-custom-engines'
It will call `websearch' with the selected region, or if no region is selected
promt the user for completion with `thing-at-point' if point is on something,
and last kill or if called with a prefix arg will bring up full `kill-ring'
history.

The function will be named websearch-ENGINE-NAME where ENGINE-NAME
corresponds to an item from `websearch-custom-engines'
DOCSTRING if supplied is applied to the variable.
If KEYBINDING is given bind the function to KEYBINDING.
If QUERY-SEPARATOR QUERY-URL and TAGS are given add them to
`websearch-custom-engines' TAGS should be passed as an unquoted list
i.e. \":tags (\"text\" \"generic\")\"

\(fn ENGINE-NAME &key DOCSTRING KEYBINDING (FUNCTION t) QUERY-SEPARATOR QUERY-URL (TAGS \\='(\"generic\")))" nil t)

(function-put 'websearch-define 'lisp-indent-function '2)

(function-put 'websearch-define 'doc-string-elt '3)

(autoload 'websearch-define-group "websearch" "\
Define a new function for searching a group of web engines using `websearch'.
function.  The function takes the following keyword arguments:

GROUP-NAME: a string that contains a list of engine names separated by
commas (e.g. \"google, duckduckgo, yandex\").

KEYBINDING: (OPTIONAL) A KEYBINDING TO BIND THE FUNCTION TO.

FUNCTION: (optional) a boolean that specifies whether the function should be
defined (defaults to t).

DOCSTRING: (optional) a string that is used as the function's documentation.

The macro first checks if the GROUP-NAME is properly formatted (i.e. that it
contains a comma) and if the group is already defined in
`websearch-custom-groups'. If the group is not properly formatted, or if the
group is not defined, the macro generates an error.

If the FUNCTION argument is t (default), the macro defines a new function called
websearch-group-GROUP-NAME where GROUP-NAME is the GROUP-NAME argument with the
commas replaced by dashes. The function takes two arguments: SEARCH-TERM and
arg. The SEARCH-TERM is the term to be searched and arg is an optional prefix
argument.

The function is interactive and prompts the user for input. If the region is
active, it uses the selected text as the search-term. If prifix-arg is passed,
it uses the kill-ring for completions. If neither is the case, it uses the
thing-at-point and the car of the kill-ring for completions.

After the user inputs a search term, the function calls `websearch--browse-url'
with the SEARCH-TERM and GROUP-NAME as arguments.

If a KEYBINDING is passed, the macro binds the newly defined function
to the specified key.

\(fn GROUP-NAME &key KEYBINDING (FUNCTION t) DOCSTRING)" nil t)

(function-put 'websearch-define-group 'lisp-indent-function '2)

(register-definition-prefixes "websearch" '("websearch-"))

;;;***

;;;### (autoloads nil "websearch-custom" "websearch-custom.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from websearch-custom.el

(register-definition-prefixes "websearch-custom" '("websearch-"))

;;;***

;;;### (autoloads nil "websearch-mode" "websearch-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from websearch-mode.el

(defvar websearch-mode nil "\
Non-nil if Websearch mode is enabled.
See the `websearch-mode' command
for a description of this minor mode.")

(custom-autoload 'websearch-mode "websearch-mode" nil)

(autoload 'websearch-mode "websearch-mode" "\
Search engine minor mode.

This is a minor mode.  If called interactively, toggle the
`Websearch mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='websearch-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'websearch-list-engines "websearch-mode" "\
Display a list of supported search engines." t nil)

(register-definition-prefixes "websearch-mode" '("websearch-"))

;;;***

;;;### (autoloads nil nil ("websearch-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; websearch-autoloads.el ends here