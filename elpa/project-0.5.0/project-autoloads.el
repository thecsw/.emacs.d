;;; project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "project" "project.el" (0 0 0 0))
;;; Generated autoloads from project.el

(autoload 'project-current "project" "\
Return the project instance in DIR or `default-directory'.
When no project found in DIR, and MAYBE-PROMPT is non-nil, ask
the user for a different project to look in.

\(fn &optional MAYBE-PROMPT DIR)" nil nil)

(defvar project-prefix-map (let ((map (make-sparse-keymap))) (define-key map "f" 'project-find-file) (define-key map "b" 'project-switch-to-buffer) (define-key map "s" 'project-shell) (define-key map "d" 'project-dired) (define-key map "v" 'project-vc-dir) (define-key map "c" 'project-compile) (define-key map "e" 'project-eshell) (define-key map "k" 'project-kill-buffers) (define-key map "p" 'project-switch-project) (define-key map "g" 'project-find-regexp) (define-key map "r" 'project-query-replace-regexp) map) "\
Keymap for project commands.")
 (define-key ctl-x-map "p" project-prefix-map)

(autoload 'project-find-regexp "project" "\
Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

\(fn REGEXP)" t nil)

(autoload 'project-or-external-find-regexp "project" "\
Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for.

\(fn REGEXP)" t nil)

(autoload 'project-find-file "project" "\
Visit a file (with completion) in the current project.
The completion default is the filename at point, if one is
recognized." t nil)

(autoload 'project-or-external-find-file "project" "\
Visit a file (with completion) in the current project or external roots.
The completion default is the filename at point, if one is
recognized." t nil)

(autoload 'project-dired "project" "\
Start Dired in the current project's root." t nil)

(autoload 'project-vc-dir "project" "\
Run VC-Dir in the current project's root." t nil)

(autoload 'project-shell "project" "\
Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists." t nil)

(autoload 'project-eshell "project" "\
Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists." t nil)

(autoload 'project-search "project" "\
Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

\(fn REGEXP)" t nil)

(autoload 'project-query-replace-regexp "project" "\
Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
If you exit the query-replace, you can later continue the query-replace
loop using the command \\[fileloop-continue].

\(fn FROM TO)" t nil)

(autoload 'project-compile "project" "\
Run `compile' in the project root.
Arguments the same as in `compile'.

\(fn COMMAND &optional COMINT)" t nil)

(autoload 'project-switch-to-buffer "project" "\
Switch to another buffer that is related to the current project.
A buffer is related to a project if `project-current' returns the
same (equal) value when called in that buffer." t nil)

(autoload 'project-kill-buffers "project" "\
Kill all live buffers belonging to the current project.
Certain buffers may be \"spared\", see `project-kill-buffers-ignores'." t nil)

(autoload 'project-known-project-roots "project" "\
Return the list of root directories of all known projects." nil nil)

(defvar project-switch-commands '((102 "Find file" project-find-file) (103 "Find regexp" project-find-regexp) (100 "Dired" project-dired) (118 "VC-Dir" project-vc-dir) (101 "Eshell" project-eshell)) "\
Alist mapping keys to project switching menu entries.
Used by `project-switch-project' to construct a dispatch menu of
commands available upon \"switching\" to another project.

Each element is of the form (KEY LABEL COMMAND), where COMMAND is the
command to run when KEY is pressed.  LABEL is used to distinguish
the menu entries in the dispatch menu.")

(autoload 'project-switch-project "project" "\
\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project" '("project-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; project-autoloads.el ends here
