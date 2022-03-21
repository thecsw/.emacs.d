;;; elpher-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elpher" "elpher.el" (0 0 0 0))
;;; Generated autoloads from elpher.el

(autoload 'elpher-bookmark-jump "elpher" "\
Handler used to open a bookmark using elpher.
The argument BOOKMARK is a bookmark record passed to the function.
This handler is responsible for loading the bookmark in some buffer,
then making that buffer the current buffer.  It should not switch
to the buffer.

\(fn BOOKMARK)" nil nil)

(autoload 'elpher-browse-url-elpher "elpher" "\
Browse URL using Elpher.  This function is used by `browse-url'.

\(fn URL &rest ARGS)" t nil)

(autoload 'elpher-go "elpher" "\
Go to a particular gopher site HOST-OR-URL.
When run interactively HOST-OR-URL is read from the minibuffer.

\(fn HOST-OR-URL)" t nil)

(autoload 'elpher "elpher" "\
Start elpher with default landing page.
The buffer used for Elpher sessions is determined by the value of
‘elpher-buffer-name’.  If there is already an Elpher session active in
that buffer, Emacs will simply switch to it.  Otherwise, a new session
will begin.  A numeric prefix ARG (as in ‘\\[universal-argument] 42
\\[execute-extended-command] elpher RET’) switches to the session with
that number, creating it if necessary.  A non numeric prefix ARG means
to create a new session.  Returns the buffer selected (or created).

\(fn &optional ARG)" t nil)

(register-definition-prefixes "elpher" '("elpher-"))

;;;***

;;;### (autoloads nil nil ("elpher-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpher-autoloads.el ends here
