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
