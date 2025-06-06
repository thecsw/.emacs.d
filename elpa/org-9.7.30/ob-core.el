;;; ob-core.el --- Working with Code Blocks          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ob-eval)
(require 'org-macs)
(require 'org-fold)
(require 'org-compat)
(require 'org-cycle)

(defconst org-babel-exeext
  (if (memq system-type '(windows-nt cygwin))
      ".exe"
    nil))

(defvar org-babel-library-of-babel)
(defvar org-edit-src-content-indentation)
(defvar org-link-file-path-type)
(defvar org-src-lang-modes)
(defvar org-babel-tangle-uncomment-comments)

(declare-function org-attach-dir "org-attach" (&optional create-if-not-exists-p no-fs-check))
(declare-function org-at-item-p "org-list" ())
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-babel-lob-execute-maybe "ob-lob" ())
(declare-function org-babel-ref-goto-headline-id "ob-ref" (id))
(declare-function org-babel-ref-headline-body "ob-ref" ())
(declare-function org-babel-ref-parse "ob-ref" (assignment))
(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-babel-tangle-comment-links "ob-tangle" (&optional info))
(declare-function org-current-level "org" ())
(declare-function org-cycle "org-cycle" (&optional arg))
(declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
(declare-function org-edit-src-exit "org-src"  ())
(declare-function org-src-preserve-indentation-p "org-src" (node))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-at-point-no-context "org-element" (&optional pom))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-normalize-string "org-element" (s))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-element-contents-begin "org-element" (node))
(declare-function org-element-contents-end "org-element" (node))
(declare-function org-element-parent "org-element-ast" (node))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function org-element-type-p "org-element-ast" (node &optional types))
(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-escape-code-in-region "org-src" (beg end))
(declare-function org-forward-heading-same-level "org" (arg &optional invisible-ok))
(declare-function org-in-commented-heading-p "org" (&optional no-inheritance))
(declare-function org-indent-block "org" ())
(declare-function org-indent-line "org" ())
(declare-function org-list-get-list-end "org-list" (item struct prevs))
(declare-function org-list-prevs-alist "org-list" (struct))
(declare-function org-list-struct "org-list" ())
(declare-function org-list-to-generic "org-list" (LIST PARAMS))
(declare-function org-list-to-lisp "org-list" (&optional delete))
(declare-function org-list-to-org "org-list" (list &optional params))
(declare-function org-macro-escape-arguments "org-macro" (&rest args))
(declare-function org-mark-ring-push "org" (&optional pos buffer))
(declare-function org-narrow-to-subtree "org" (&optional element))
(declare-function org-next-block "org" (arg &optional backward block-regexp))
(declare-function org-open-at-point "org" (&optional in-emacs reference-buffer))
(declare-function org-previous-block "org" (arg &optional block-regexp))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-src-coderef-format "org-src" (&optional element))
(declare-function org-src-coderef-regexp "org-src" (fmt &optional label))
(declare-function org-src-get-lang-mode "org-src" (lang))
(declare-function org-table-align "org-table" ())
(declare-function org-table-convert-region "org-table" (beg0 end0 &optional separator))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function org-table-import "org-table" (file arg))
(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-unescape-code-in-string "org-src" (s))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function tramp-compat-make-temp-file "tramp-compat" (filename &optional dir-flag))

(defgroup org-babel nil
  "Code block evaluation and management in `org-mode' documents."
  :tag "Babel"
  :group 'org)

(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
\\<org-mode-map>\
Require confirmation before interactively evaluating code
blocks in Org buffers.  The default value of this variable is t,
meaning confirmation is required for any code block evaluation.
This variable can be set to nil to inhibit any future
confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.  Such a function should then
return a non-nil value if the user should be prompted for
execution or nil if no prompt is required.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code.  It may be advisable
remove code block execution from `\\[org-ctrl-c-ctrl-c]' \
as further protection
against accidental code block evaluation.  The
`org-babel-no-eval-on-ctrl-c-ctrl-c' variable can be used to
remove code block execution from the `\\[org-ctrl-c-ctrl-c]' keybinding."
  :group 'org-babel
  :version "24.1"
  :type '(choice boolean function))
;; don't allow this variable to be changed through file settings
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (x) (eq x t)))

(defcustom org-babel-no-eval-on-ctrl-c-ctrl-c nil
  "\\<org-mode-map>\
Remove code block evaluation from the `\\[org-ctrl-c-ctrl-c]' key binding."
  :group 'org-babel
  :version "24.1"
  :type 'boolean)

(defcustom org-babel-results-keyword "RESULTS"
  "Keyword used to name results generated by code blocks.
It should be \"RESULTS\".  However any capitalization may be
used."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string
  :safe (lambda (v)
	  (and (stringp v)
	       (org-string-equal-ignore-case "RESULTS" v))))

(defcustom org-babel-noweb-wrap-start "<<"
  "String used to begin a noweb reference in a code block.
See also `org-babel-noweb-wrap-end'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-noweb-wrap-end ">>"
  "String used to end a noweb reference in a code block.
See also `org-babel-noweb-wrap-start'."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-inline-result-wrap "=%s="
  "Format string used to wrap inline results.
This string must include a \"%s\" which will be replaced by the results."
  :group 'org-babel
  :type 'string)
(put 'org-babel-inline-result-wrap
     'safe-local-variable
     (lambda (value)
       (and (stringp value)
	    (string-match-p "%s" value))))

(defcustom org-babel-hash-show-time nil
  "Non-nil means show the time the code block was evaluated in the result hash."
  :group 'org-babel
  :type 'boolean
  :package-version '(Org . "9.0")
  :safe #'booleanp)

(defcustom org-babel-uppercase-example-markers nil
  "When non-nil, begin/end example markers will be inserted in upper case."
  :group 'org-babel
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.1")
  :safe #'booleanp)

(defun org-babel-noweb-wrap (&optional regexp)
  "Return regexp matching a Noweb reference.

Match any reference, or only those matching REGEXP, if non-nil.

When matching, reference is stored in match group 1."
  (concat (regexp-quote org-babel-noweb-wrap-start)
	  (or regexp "\\([^ \t\n]\\(?:.*?[^ \t\n]\\)?\\)")
	  (regexp-quote org-babel-noweb-wrap-end)))

(defvar org-babel-src-name-regexp
  "^[ \t]*#\\+name:[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-multi-line-header-regexp
  "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
  "Regular expression used to match multi-line header arguments.")

(defvar org-babel-src-block-regexp
  (concat
   ;; (1) indentation                 (2) lang
   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
   ;; (3) switches
   "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
   ;; (4) header arguments
   "\\([^\n]*\\)\n"
   ;; (5) body
   "\\(\\(?:.\\|\n\\)*?\n\\)??[ \t]*#\\+end_src")
  "Regexp used to identify code blocks.")

(defun org-babel--get-vars (params)
  "Return the babel variable assignments in PARAMS.

PARAMS is a quasi-alist of header args, which may contain
multiple entries for the key `:var'.  This function returns a
list of the cdr of all the `:var' entries."
  (mapcar #'cdr
	  (cl-remove-if-not (lambda (x) (eq (car x) :var)) params)))

(defvar org-babel-exp-reference-buffer nil
  "Buffer containing original contents of the exported buffer.
This is used by Babel to resolve references in source blocks.
Its value is dynamically bound during export.")

(defun org-babel-check-confirm-evaluate (info)
  "Check whether INFO allows code block evaluation.

Returns nil if evaluation is disallowed, t if it is
unconditionally allowed, and the symbol `query' if the user
should be asked whether to allow evaluation."
  (let* ((headers (nth 2 info))
	 (eval (or (cdr  (assq :eval headers))
		   (when (assq :noeval headers) "no")))
	 (eval-no (member eval '("no" "never")))
	 (export org-babel-exp-reference-buffer)
	 (eval-no-export (and export (member eval '("no-export" "never-export"))))
	 (noeval (or eval-no eval-no-export))
	 (query (or (equal eval "query")
		    (and export (equal eval "query-export"))
		    (if (functionp org-confirm-babel-evaluate)
			(funcall org-confirm-babel-evaluate
				 ;; Language, code block body.
				 (nth 0 info)
				 (org-babel--expand-body info))
		      org-confirm-babel-evaluate))))
    (cond
     (noeval nil)
     (query 'query)
     (t t))))

(defun org-babel-check-evaluate (info)
  "Check if code block INFO should be evaluated.
Do not query the user, but do display an informative message if
evaluation is blocked.  Returns non-nil if evaluation is not blocked."
  (let ((confirmed (org-babel-check-confirm-evaluate info)))
    (unless confirmed
      (message "Evaluation of this %s code block%sis disabled."
	       (nth 0 info)
	       (let ((name (nth 4 info)))
		 (if name (format " (%s) " name) " "))))
    confirmed))

;; Dynamically scoped for asynchronous export.
(defvar org-babel-confirm-evaluate-answer-no)

(defun org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.

This query can also be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code.

The variable `org-babel-confirm-evaluate-answer-no' is used by
the async export process, which requires a non-interactive
environment, to override this check."
  (let* ((evalp (org-babel-check-confirm-evaluate info))
	 (lang (nth 0 info))
	 (name (nth 4 info))
	 (name-string (if name (format " (%s) " name) " ")))
    (pcase evalp
      (`nil nil)
      (`t t)
      (`query (or
	       (and (not (bound-and-true-p
			  org-babel-confirm-evaluate-answer-no))
		    (yes-or-no-p
		     (format "Evaluate this %s code block%son your system? "
			     lang name-string)))
	       (progn
		 (message "Evaluation of this %s code block%sis aborted."
			  lang name-string)
		 nil)))
      (x (error "Unexpected value `%s' from `org-babel-check-confirm-evaluate'" x)))))

;;;###autoload
(defun org-babel-execute-safely-maybe ()
  "Maybe `org-babel-execute-maybe'.
This function does nothing unless `org-babel-no-eval-on-ctrl-c-ctrl-c'
is non-nil."
  (unless org-babel-no-eval-on-ctrl-c-ctrl-c
    (org-babel-execute-maybe)))

;;;###autoload
(defun org-babel-execute-maybe ()
"Execute src block or babel call at point."
  (interactive)
  (or (org-babel-execute-src-block-maybe)
      (org-babel-lob-execute-maybe)))

(defmacro org-babel-when-in-src-block (&rest body)
  "Execute BODY if point is in a source block and return t.

Otherwise do nothing and return nil."
  `(if (org-element-type-p (org-element-context) '(inline-src-block src-block))
       (progn
	 ,@body
	 t)
     nil))

(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-eval-wipe-error-buffer)
   (org-babel-execute-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-view-src-block-info ()
  "Display information on the current source block.
This includes header arguments, language and name, and is largely
a window into the `org-babel-get-src-block-info' function."
  (interactive)
  (let ((info (org-babel-get-src-block-info 'no-eval))
	(full (lambda (it) (> (length it) 0)))
	(printf (lambda (fmt &rest args) (princ (apply #'format fmt args)))))
    (when info
      (let* ((name        (nth 4 info))
	     (language    (nth 0 info))
	     (switches    (nth 3 info))
	     (header-args (nth 2 info))
	     (property-header-args
              (org-entry-get (point) "header-args" t))
             (property-header-args-language
              (org-entry-get (point) (concat "header-args:" language) t)))
	(with-help-window (help-buffer)
	  (when name            (funcall printf "Name: %s\n"     name))
	  (when language        (funcall printf "Language: %s\n"     language))
          ;; Show header arguments that have been set through
          ;; properties (i.e. in property drawers or through
          ;; #+PROPERTY)
	  (funcall printf "Properties:\n")
	  (funcall printf "\t:header-args \t%s\n" property-header-args)
	  (funcall printf "\t:header-args:%s \t%s\n" language property-header-args-language)
          ;; Show switches
	  (when (funcall full switches) (funcall printf "Switches: %s\n" switches))
          ;; Show default header arguments and header arguments that
          ;; have been explicitly set in the current code block.
	  (funcall printf "Header Arguments:\n")
	  (dolist (pair (sort header-args
			      (lambda (a b) (string< (symbol-name (car a))
						     (symbol-name (car b))))))
	    (when (funcall full (format "%s" (cdr pair)))
	      (funcall printf "\t%S%s\t%s\n"
		       (car pair)
		       (if (> (length (format "%S" (car pair))) 7) "" "\t")
		       (cdr pair)))))))))

;;;###autoload
(defun org-babel-expand-src-block-maybe ()
  "Conditionally expand a source block.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-expand-src-block'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-expand-src-block current-prefix-arg)))

;;;###autoload
(defun org-babel-load-in-session-maybe ()
  "Conditionally load a source block in a session.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-load-in-session current-prefix-arg)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

;;;###autoload
(defun org-babel-pop-to-session-maybe ()
  "Conditionally pop to a session.
Detect if this is context for an org-babel src-block and if so
then run `org-babel-switch-to-session'."
  (interactive)
  (org-babel-when-in-src-block
   (org-babel-switch-to-session current-prefix-arg)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defconst org-babel-common-header-args-w-values
  '((cache	. ((no yes)))
    (cmdline	. :any)
    (colnames	. ((nil no yes)))
    (comments	. ((no link yes org both noweb)))
    (dir	. :any)
    (eval	. ((yes no no-export strip-export never-export eval never
			query)))
    (exports	. ((code results both none)))
    (epilogue   . :any)
    (file	. :any)
    (file-desc  . :any)
    (file-ext   . :any)
    (file-mode  . ((#o755 #o555 #o444 :any)))
    (hlines	. ((no yes)))
    (mkdirp	. ((yes no)))
    (no-expand)
    (noeval)
    (noweb	. ((yes no tangle strip-tangle no-export strip-export)))
    (noweb-ref	. :any)
    (noweb-sep  . :any)
    (noweb-prefix . ((no yes)))
    (output-dir . :any)
    (padline	. ((yes no)))
    (post       . :any)
    (prologue   . :any)
    (results	. ((file list vector table scalar verbatim)
		   (raw html latex org code pp drawer link graphics)
		   (replace silent none discard append prepend)
		   (output value)))
    (rownames	. ((no yes)))
    (sep	. :any)
    (session	. :any)
    (shebang	. :any)
    (tangle	. ((tangle yes no :any)))
    (tangle-mode . ((#o755 #o555 #o444 :any)))
    (var	. :any)
    (wrap       . :any))
  "Alist defining common header args and their allowed values.

Keys of the alist are header arg symbols.
Values of the alist are either a symbol `:any' or a list of allowed
values as symbols:

   (header-name . :any)
   (header-name . ((value1 value2 value3 ...))
   (header-name . ((value1 value2 value3 ... :any))

When Org considers header-arg property inheritance, the innermost
value from the list is considered.

Symbol `:any' in the value list implies that any value is allowed.
Yet the explicitly listed values from the list will be offered as
completion candidates.

FIXME: This is currently just supported for `results' and `exports'.
Values in the alist can also be a list of lists.  The inner lists
define exclusive groups of values that can be set at the same time for
a given header argument.

  (results . ((file list ...)
             (raw html ...))

The above example allows multi-component header arguments like

   #+begin_src bash :results file raw
   <:results will combine the two values \"file raw\".>

   #+begin_src bash :results file list
   <:results will only use the last value \"list\".>

   #+property: header-args :results file html
   ...
   #+begin_src bash :results list
   <:results will inherit with partial override \"list html\".>

See info node `(org)Results of evaluation' for more details.")

(defconst org-babel-header-arg-names
  (mapcar #'car org-babel-common-header-args-w-values)
  "Common header arguments used by org-babel.
Note that individual languages may define their own language
specific header arguments as well.")

(defconst org-babel-safe-header-args
  '(:cache :colnames :comments :exports :epilogue :hlines :noeval
	   :noweb :noweb-ref :noweb-sep :noweb-prefix :padline
           :prologue :rownames :sep :session :tangle :wrap
	   (:eval . ("never" "query"))
	   (:results . (lambda (str) (not (string-match "file" str)))))
  "A list of safe header arguments for babel source blocks.

The list can have entries of the following forms:
- :ARG                     -> :ARG is always a safe header arg
- (:ARG . (VAL1 VAL2 ...)) -> :ARG is safe as a header arg if it is
                              `equal' to one of the VALs.
- (:ARG . FN)              -> :ARG is safe as a header arg if the function FN
                              returns non-nil.  FN is passed one
                              argument, the value of the header arg
                              (as a string).")

(defmacro org-babel-header-args-safe-fn (safe-list)
  "Return a function that determines whether a list of header args are safe.

Intended usage is:
\(put \\='org-babel-default-header-args \\='safe-local-variable
 (org-babel-header-args-safe-p org-babel-safe-header-args)

This allows org-babel languages to extend the list of safe values for
their `org-babel-default-header-args:foo' variable.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  `(lambda (value)
     (and (listp value)
	  (cl-every
	   (lambda (pair)
	     (and (consp pair)
		  (org-babel-one-header-arg-safe-p pair ,safe-list)))
	   value))))

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code")
    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
  "Default arguments to use when evaluating a source block.

This is a list in which each element is an alist.  Each key
corresponds to a header argument, and each value to that header's
value.  The value can either be a string or a closure that
evaluates to a string.

A closure is evaluated when the source block is being
evaluated (e.g. during execution or export), with point at the
source block.  It is not possible to use an arbitrary function
symbol (e.g. `some-func'), since org uses lexical binding.  To
achieve the same functionality, call the function within a
closure (e.g. (lambda () (some-func))).

To understand how closures can be used as default header
arguments, imagine you'd like to set the file name output of a
latex source block to a sha1 of its contents.  We could achieve
this with:

  (defun org-src-sha ()
    (let ((elem (org-element-at-point)))
      (concat (sha1 (org-element-property :value elem)) \".svg\")))

  (setq org-babel-default-header-args:latex
        `((:results . \"file link replace\")
          (:file . (lambda () (org-src-sha)))))

Because the closure is evaluated with point at the source block,
the call to `org-element-at-point' above will always retrieve
information about the current source block.

Some header arguments can be provided multiple times for a source
block.  An example of such a header argument is :var.  This
functionality is also supported for default header arguments by
providing the header argument multiple times in the alist.  For
example:

 ((:var . \"foo=\\\"bar\\\"\")
  (:var . \"bar=\\\"foo\\\"\"))")

(put 'org-babel-default-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "replace")
    (:exports . "results") (:hlines . "yes"))
  "Default arguments to use when evaluating an inline source block.")
(put 'org-babel-default-inline-header-args 'safe-local-variable
     (org-babel-header-args-safe-fn org-babel-safe-header-args))

(defconst org-babel-name-regexp
  (format "^[ \t]*#\\+%s:[ \t]*"
	  ;; FIXME: TBLNAME is for backward compatibility.
	  (regexp-opt '("NAME" "TBLNAME")))
  "Regexp matching a NAME keyword.")

(defconst org-babel-result-regexp
  (rx (seq bol
           (zero-or-more (any "\t "))
           "#+results"
           (opt "["
		;; Time stamp part.
		(opt "("
                     (= 4 digit) (= 2 "-" (= 2 digit))
                     " "
                     (= 2 digit) (= 2 ":" (= 2 digit))
                     ") ")
		;; SHA1 hash.
		(group (one-or-more hex-digit))
		"]")
           ":"
           (zero-or-more (any "\t "))))
  "Regular expression used to match result lines.
If the results are associated with a hash key then the hash will
be saved in match group 1.")

(defconst org-babel-result-w-name-regexp
  (concat org-babel-result-regexp "\\(?9:[^ \t\n\r\v\f]+\\)")
  "Regexp matching a RESULTS keyword with a name.
Name is saved in match group 9.")

(defvar org-babel-min-lines-for-block-output 10
  "The minimum number of lines for block output.
If number of lines of output is equal to or exceeds this
value, the output is placed in a #+begin_example...#+end_example
block.  Otherwise the output is marked as literal by inserting
colons at the starts of the lines.  This variable only takes
effect if the :results output option is in effect.")

(defvar org-babel-noweb-error-all-langs nil
  "Raise errors when noweb references don't resolve.
Also see `org-babel-noweb-error-langs' to control noweb errors on
a language by language bases.")

(defvar org-babel-noweb-error-langs nil
  "Languages for which Babel will raise literate programming errors.
List of languages for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.  Also see `org-babel-noweb-error-all-langs'
to raise errors for all languages.")

(defvar org-babel-hash-show 4
  "Number of initial characters to show of a hidden results hash.")

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'.")

(defun org-babel-named-src-block-regexp-for-name (&optional name)
  "Generate a regexp used to match a source block named NAME.
If NAME is nil, match any name.  Matched name is then put in
match group 9.  Other match groups are defined in
`org-babel-src-block-regexp'."
  (concat org-babel-src-name-regexp
	  (concat (if name (regexp-quote name) "\\(?9:.*?\\)") "[ \t]*" )
	  "\\(?:\n[ \t]*#\\+\\S-+:.*\\)*?"
	  "\n"
	  (substring org-babel-src-block-regexp 1)))

(defun org-babel-named-data-regexp-for-name (name)
  "Generate a regexp used to match data named NAME."
  (concat org-babel-name-regexp (regexp-quote name) "[ \t]*$"))

(defun org-babel--normalize-body (datum)
  "Normalize body for element or object DATUM.
DATUM is a source block element or an inline source block object.
Remove final newline character and spurious indentation."
  (let* ((value (org-element-property :value datum))
	 (body (if (string-suffix-p "\n" value)
		   (substring value 0 -1)
		 value)))
    (cond ((org-element-type-p datum 'inline-src-block)
	   ;; Newline characters and indentation in an inline
	   ;; src-block are not meaningful, since they could come from
	   ;; some paragraph filling.  Treat them as a white space.
	   (replace-regexp-in-string "\n[ \t]*" " " body))
	  ((org-src-preserve-indentation-p datum) body)
	  (t (org-remove-indentation body)))))

;;; functions
(defvar org-babel-current-src-block-location nil
  "Marker pointing to the source block currently being executed.
This may also point to a call line or an inline code block.  If
multiple blocks are being executed (e.g., in chained execution
through use of the :var header argument) this marker points to
the outer-most code block.")

(defun org-babel-eval-headers (headers)
  "Compute header list set with HEADERS.

Evaluate all header arguments set to functions prior to returning
the list of header arguments."
  (let ((lst nil))
    (dolist (elem headers)
      (if (and (cdr elem) (functionp (cdr elem)))
          (push `(,(car elem) . ,(funcall (cdr elem))) lst)
        (push elem lst)))
    (reverse lst)))

(defun org-babel-get-src-block-info (&optional no-eval datum)
  "Extract information from a source block or inline source block.

When optional argument NO-EVAL is non-nil, Babel does not resolve
remote variable references; a process which could likely result
in the execution of other code blocks, and do not evaluate Lisp
values in parameters.

By default, consider the block at point.  However, when optional
argument DATUM is provided, extract information from that parsed
object instead.

Return nil if point is not on a source block (blank lines after a
source block are considered a part of that source block).
Otherwise, return a list with the following pattern:

  (language body arguments switches name start coderef)"
  (let* ((datum (or datum (org-element-context)))
	 (type (org-element-type datum))
	 (inline (eq type 'inline-src-block)))
    (when (memq type '(inline-src-block src-block))
      (let* ((lang (org-element-property :language datum))
	     (lang-headers (intern
			    (concat "org-babel-default-header-args:" lang)))
	     (name (org-element-property :name datum))
	     (info
	      (list
	       lang
	       (org-babel--normalize-body datum)
	       (apply #'org-babel-merge-params
                      ;; Use `copy-tree' to avoid creating shared structure
                      ;; with the `org-babel-default-header-args-*' variables:
                      ;; modifications by `org-babel-generate-file-param'
                      ;; below would modify the shared structure, thereby
                      ;; modifying the variables.
                      (copy-tree
                       (if inline org-babel-default-inline-header-args
                         org-babel-default-header-args)
                       t)
                      (and (boundp lang-headers)
                           (copy-tree (eval lang-headers t) t))
		      (append
		       ;; If DATUM is provided, make sure we get node
		       ;; properties applicable to its location within
		       ;; the document.
		       (org-with-point-at (org-element-begin datum)
			 (org-babel-params-from-properties lang no-eval))
		       (mapcar (lambda (h)
				 (org-babel-parse-header-arguments h no-eval))
			       (cons (org-element-property :parameters datum)
				     (org-element-property :header datum)))))
	       (or (org-element-property :switches datum) "")
	       name
	       (org-element-property (if inline :begin :post-affiliated)
				     datum)
	       (and (not inline) (org-src-coderef-format datum)))))
	(unless no-eval
	  (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
	(setf (nth 2 info) (org-babel-generate-file-param name (nth 2 info)))
	info))))

(defun org-babel--expand-body (info)
  "Expand noweb references in src block and remove any coderefs.
The src block is defined by its INFO, as returned by
`org-babel-get-src-block-info'."
  (let ((coderef (nth 6 info))
	(expand
	 (if (org-babel-noweb-p (nth 2 info) :eval)
	     (org-babel-expand-noweb-references info)
	   (nth 1 info))))
    (if (not coderef) expand
      (replace-regexp-in-string
       (org-src-coderef-regexp coderef) "" expand nil nil 1))))

(defun org-babel--file-desc (params result)
  "Retrieve description for file link result of evaluation.
PARAMS is header argument values.  RESULT is the file link as returned
by the code block.

When `:file-desc' header argument is provided use its value or
duplicate RESULT in the description.

When `:file-desc' is missing, return nil."
  (pcase (assq :file-desc params)
    (`nil nil)
    (`(:file-desc) result)
    (`(:file-desc . ,(and (pred stringp) val)) val)))

(defvar *this*)
;; Dynamically bound in `org-babel-execute-src-block'
;; and `org-babel-read'

(defun org-babel-session-buffer (&optional info)
  "Return buffer name for session associated with current code block.
Return nil when no such live buffer with process exists.
When INFO is non-nil, it should be a list returned by
`org-babel-get-src-block-info'.
This function uses org-babel-session-buffer:<lang> function to
retrieve backend-specific session buffer name."
  (declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
  (when-let* ((info (or info (org-babel-get-src-block-info 'no-eval)))
              (lang (nth 0 info))
              (session (cdr (assq :session (nth 2 info))))
              (cmd (intern (concat "org-babel-session-buffer:" lang)))
              (buffer-name
               (if (fboundp cmd)
                   (funcall cmd session info)
                 ;; Use session name as buffer name by default.
                 session)))
    (require 'ob-comint)
    (when (org-babel-comint-buffer-livep buffer-name)
      buffer-name)))

;;;###autoload
(defun org-babel-execute-src-block (&optional arg info params executor-type)
  "Execute the current source code block and return the result.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block.

EXECUTOR-TYPE is the type of the org element responsible for the
execution of the source block.  If not provided then informed
guess will be made."
  (interactive)
  (let* ((org-babel-current-src-block-location
          (or org-babel-current-src-block-location
              (nth 5 info)
              (org-babel-where-is-src-block-head)))
         (info (if info (copy-tree info) (org-babel-get-src-block-info)))
         (executor-type
          (or executor-type
              ;; If `executor-type' is unset, then we will make an
              ;; informed guess.
              (pcase (and
                      ;; When executing virtual src block, no location
                      ;; is known.
                      org-babel-current-src-block-location
                      (char-after org-babel-current-src-block-location))
                (?s 'inline-src-block)
                (?c 'inline-babel-call)
                (?# (pcase (char-after (+ 2 org-babel-current-src-block-location))
                      (?b 'src-block)
                      (?c 'call-block)
                      (_ 'unknown)))
                (_ 'unknown)))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
	     (cache (let ((c (cdr (assq :cache params))))
		      (and (not arg) c (string= "yes" c))))
	     (new-hash (and cache (org-babel-sha1-hash info :eval)))
	     (old-hash (and cache (org-babel-current-result-hash)))
	     (current-cache (and new-hash (equal new-hash old-hash))))
	(cond
	 (current-cache
	  (save-excursion		;Return cached result.
	    (goto-char (org-babel-where-is-src-block-result nil info))
	    (forward-line)
	    (skip-chars-forward " \t")
	    (let ((result (org-babel-read-result)))
              (unless noninteractive
	        (message (format "Cached: %s"
                                 (replace-regexp-in-string "%" "%%" (format "%S" result)))))
	      result)))
	 ((org-babel-confirm-evaluate info)
	  (let* ((lang (nth 0 info))
		 (result-params (cdr (assq :result-params params)))
		 (body (org-babel--expand-body info))
		 (dir (cdr (assq :dir params)))
		 (mkdirp (cdr (assq :mkdirp params)))
		 (default-directory
		  (cond
		   ((not dir) default-directory)
                   ((when-let* ((session (org-babel-session-buffer info)))
                      (buffer-local-value 'default-directory (get-buffer session))))
		   ((member mkdirp '("no" "nil" nil))
		    (file-name-as-directory (expand-file-name dir)))
		   (t
		    (let ((d (file-name-as-directory (expand-file-name dir))))
		      (make-directory d 'parents)
		      d))))
		 (cmd (intern (concat "org-babel-execute:" lang)))
		 result exec-start-time)
	    (unless (fboundp cmd)
	      (error "No org-babel-execute function for %s!" lang))
            (unless noninteractive
	      (message "Executing %s %s %s..."
		       (capitalize lang)
                       (pcase executor-type
                         ('src-block "code block")
                         ('inline-src-block "inline code block")
                         ('babel-call "call")
                         ('inline-babel-call "inline call")
                         (e (symbol-name e)))
		       (let ((name (nth 4 info)))
		         (if name
                             (format "(%s)" name)
                           (format "at position %S" (nth 5 info))))))
	    (setq exec-start-time (current-time)
                  result
		  (let ((r
                         ;; Code block may move point in the buffer.
                         ;; Make sure that the point remains on the
                         ;; code block.
                         (save-excursion (funcall cmd body params))))
		    (if (and (eq (cdr (assq :result-type params)) 'value)
			     (or (member "vector" result-params)
				 (member "table" result-params))
			     (not (listp r)))
			(list (list r))
		      r)))
	    (let ((file (and (member "file" result-params)
			     (cdr (assq :file params)))))
	      ;; If non-empty result and :file then write to :file.
	      (when file
		;; If `:results' are special types like `link' or
		;; `graphics', don't write result to `:file'.  Only
		;; insert a link to `:file'.
		(when (and result
			   (not (or (member "link" result-params)
				    (member "graphics" result-params))))
		  (with-temp-file file
		    (insert (org-babel-format-result
			     result
			     (cdr (assq :sep params)))))
		  ;; Set file permissions if header argument
		  ;; `:file-mode' is provided.
		  (when (assq :file-mode params)
		    (set-file-modes file (cdr (assq :file-mode params)))))
		(setq result file))
	      ;; Possibly perform post process provided its
	      ;; appropriate.  Dynamically bind "*this*" to the
	      ;; actual results of the block.
	      (let ((post (cdr (assq :post params))))
		(when post
		  (let ((*this* (if (not file) result
				  (org-babel-result-to-file
				   file
				   (org-babel--file-desc params result)
                                   'attachment))))
		    (setq result (org-babel-ref-resolve post))
		    (when file
		      (setq result-params (remove "file" result-params))))))
	      (unless (member "none" result-params)
	        (org-babel-insert-result
	         result result-params info
                 ;; append/prepend cannot handle hash as we accumulate
                 ;; multiple outputs together.
                 (when (member "replace" result-params) new-hash)
                 lang
                 (time-subtract (current-time) exec-start-time))))
	    (run-hooks 'org-babel-after-execute-hook)
	    result)))))))

(defun org-babel-expand-body:generic (body params &optional var-lines)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to its header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function.

VAR-LINES is a list of lines that define variable environment.  These
lines will be added after `:prologue' parameter and before BODY."
  (let ((pro (cdr (assq :prologue params)))
	(epi (cdr (assq :epilogue params))))
    (mapconcat #'identity
	       (append (when pro (list pro))
		       var-lines
		       (list body)
		       (when epi (list epi)))
	       "\n")))

;;;###autoload
(defun org-babel-expand-src-block (&optional _arg info params)
  "Expand the current source code block or block specified by INFO.
INFO is the output of `org-babel-get-src-block-info'.
PARAMS defines inherited header arguments.

Expand according to the source code block's header
arguments and pop open the results in a preview buffer."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
							(symbol-name (car el2)))))))
         (body (setf (nth 1 info)
		     (if (org-babel-noweb-p params :eval)
			 (org-babel-expand-noweb-references info) (nth 1 info))))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	 (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					  lang)))
         (expanded
	  (if (fboundp expand-cmd) (funcall expand-cmd body params)
	    (org-babel-expand-body:generic
	     body params (and (fboundp assignments-cmd)
			      (funcall assignments-cmd params))))))
    (if (called-interactively-p 'any)
	(org-edit-src-code
	 expanded (concat "*Org-Babel Preview " (buffer-name) "[ " lang " ]*"))
      expanded)))

(defun org-babel-combine-header-arg-lists (original &rest others)
  "Combine ORIGINAL and OTHERS lists of header argument names and arguments."
  (let ((results (copy-sequence original)))
    (dolist (new-list others)
      (dolist (arg-pair new-list)
	(let ((header (car arg-pair)))
	  (setq results
		(cons arg-pair (cl-remove-if
				(lambda (pair) (equal header (car pair)))
				results))))))
    results))

;;;###autoload
(defun org-babel-check-src-block ()
  "Check for misspelled header arguments in the current code block."
  (interactive)
  ;; TODO: report malformed code block
  ;; TODO: report incompatible combinations of header arguments
  ;; TODO: report uninitialized variables
  (let ((too-close 2) ;; <- control closeness to report potential match
	(names (mapcar #'symbol-name org-babel-header-arg-names)))
    (dolist (header (mapcar (lambda (arg) (substring (symbol-name (car arg)) 1))
			    (and (org-babel-where-is-src-block-head)
				 (org-babel-parse-header-arguments
				  (org-no-properties
				   (match-string 4))))))
      (dolist (name names)
	(when (and (not (string= header name))
		   (<= (org-string-distance header name) too-close)
		   (not (member header names)))
	  (error "Supplied header \"%S\" is suspiciously close to \"%S\""
		 header name))))
    (message "No suspicious header arguments found.")))

;;;###autoload
(defun org-babel-insert-header-arg (&optional header-arg value)
  "Insert a header argument and its value.
HEADER-ARG and VALUE, when provided, are the header argument name and
its value.  When HEADER-ARG or VALUE are nil, offer interactive
completion from lists of common args and values."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'no-eval))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers) (eval lang-headers t))))
	 (header-arg (or header-arg
			 (completing-read
			  "Header Arg: "
			  (mapcar
			   (lambda (header-spec) (symbol-name (car header-spec)))
			   headers))))
	 (vals (cdr (assoc (intern header-arg) headers)))
	 (value (or value
		    (cond
		     ((eq vals :any)
		      (read-from-minibuffer "value: "))
		     ((listp vals)
		      (mapconcat
		       (lambda (group)
			 (let ((arg (completing-read
				     "Value: "
				     (cons "default"
					   (mapcar #'symbol-name group)))))
			   (if (and arg (not (string= "default" arg)))
			       (concat arg " ")
			     "")))
		       vals ""))))))
    (save-excursion
      (goto-char begin)
      (goto-char (line-end-position))
      (unless (= (char-before (point)) ?\ ) (insert " "))
      (insert ":" header-arg) (when value (insert " " value)))))

;; Add support for completing-read insertion of header arguments after ":"
(defun org-babel-header-arg-expand ()
  "Call `org-babel-enter-header-arg-w-completion' in appropriate contexts."
  (when (and (equal (char-before) ?\:) (org-babel-where-is-src-block-head))
    (org-babel-enter-header-arg-w-completion (match-string 2))))

(defun org-babel-enter-header-arg-w-completion (&optional lang)
  "Insert header argument appropriate for LANG with completion."
  (let* ((lang-headers-var (intern (concat "org-babel-header-args:" lang)))
         (lang-headers (when (boundp lang-headers-var) (eval lang-headers-var t)))
	 (headers-w-values (org-babel-combine-header-arg-lists
			    org-babel-common-header-args-w-values lang-headers))
         (headers (mapcar #'symbol-name (mapcar #'car headers-w-values)))
         (header (org-completing-read "Header Arg: " headers))
         (args (cdr (assoc (intern header) headers-w-values)))
         (arg (when (and args (listp args))
                (org-completing-read
                 (format "%s: " header)
                 (mapcar #'symbol-name (apply #'append args))))))
    (insert (concat header " " (or arg "")))
    (cons header arg)))

(add-hook 'org-cycle-tab-first-hook 'org-babel-header-arg-expand)

;;;###autoload
(defun org-babel-load-in-session (&optional _arg info)
  "Load the body of the current source-code block.
When optional argument INFO is non-nil, use source block defined in
INFO, as returned by `org-babel-get-src-block-info'.

Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (if (not info)
		   (user-error "No src code block at point")
		 (setf (nth 1 info)
		       (if (org-babel-noweb-p params :eval)
			   (org-babel-expand-noweb-references info)
			 (nth 1 info)))))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-initiate-session (&optional arg info)
  "Initiate session for current code block or the block defined by INFO.
If called with a prefix argument ARG, then resolve any variable
references in the header arguments and assign these variables in
the session.  Copy the body of the code block to the kill ring."
  (interactive "P")
  (let* ((info (or info (org-babel-get-src-block-info (not arg))))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assq :session params)))
	 (dir (cdr (assq :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (prep-cmd (intern (concat "org-babel-prep-session:" lang))))
    (when (and (stringp session) (string= session "none"))
      (error "This block is not using a session!"))
    (unless (fboundp init-cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    (with-temp-buffer (insert (org-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    (when arg
      (unless (fboundp prep-cmd)
	(error "No org-babel-prep-session function for %s!" lang))
      (funcall prep-cmd session params))
    (funcall init-cmd session params)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block or block defined by INFO.
Uses `org-babel-initiate-session' to start the session.  If called
with a prefix argument ARG, then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (pop-to-buffer (org-babel-initiate-session arg info))
  (end-of-line 1))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

(defvar org-src-window-setup)

;;;###autoload
(defun org-babel-switch-to-session-with-code (&optional arg _info)
  "Switch to code buffer and display session.
Prefix argument ARG is passed to `org-babel-switch-to-session'."
  (interactive "P")
  (let ((swap-windows
	 (lambda ()
	   (let ((other-window-buffer (window-buffer (next-window))))
	     (set-window-buffer (next-window) (current-buffer))
	     (set-window-buffer (selected-window) other-window-buffer))
	   (other-window 1)))
	(info (org-babel-get-src-block-info))
	(org-src-window-setup 'reorganize-frame))
    (save-excursion
      (org-babel-switch-to-session arg info))
    (org-edit-src-code)
    (funcall swap-windows)))

;;;###autoload
(defmacro org-babel-do-in-edit-buffer (&rest body)
  "Evaluate BODY in edit buffer if there is a code block at point.
Return t if a code block was found at point, nil otherwise."
  (declare (debug (body)))
  `(let* ((element (org-element-at-point))
	  ;; This function is not supposed to move point.  However,
	  ;; `org-edit-src-code' always moves point back into the
	  ;; source block.  It is problematic if the point was before
	  ;; the code, e.g., on block's opening line.  In this case,
	  ;; we want to restore this location after executing BODY.
	  (outside-position
	   (and (<= (line-beginning-position)
		   (org-element-post-affiliated element))
		(point-marker)))
	  (org-src-window-setup 'switch-invisibly))
     (when (and (org-babel-where-is-src-block-head element)
		(condition-case nil
                    (org-edit-src-code)
                  (t
                   (org-edit-src-exit)
                   (when outside-position (goto-char outside-position))
                   nil)))
       (unwind-protect (progn ,@body)
	 (org-edit-src-exit)
	 (when outside-position (goto-char outside-position)))
       t)))

(defun org-babel-do-key-sequence-in-edit-buffer (key)
  "Read key sequence KEY and execute the command in edit buffer.
Enter a key sequence to be executed in the language major-mode
edit buffer.  For example, TAB will alter the contents of the
Org code block according to the effect of TAB in the language
major mode buffer.  For languages that support interactive
sessions, this can be used to send code from the Org buffer
to the session for evaluation using the native major mode
evaluation mechanisms."
  (interactive "kEnter key-sequence to execute in edit buffer: ")
  (org-babel-do-in-edit-buffer
   (call-interactively
    (key-binding (or key (read-key-sequence nil))))))

(defvar org-link-bracket-re)

(defun org-babel-active-location-p ()
  "Return non-nil, when at executable element."
  (org-element-type-p
   (save-match-data (org-element-context))
   '(babel-call inline-babel-call inline-src-block src-block)))

;;;###autoload
(defun org-babel-open-src-block-result (&optional re-run)
  "Open results of source block at point.

If `point' is on a source block then open the results of the source
code block, otherwise return nil.  With optional prefix argument
RE-RUN the source-code block is evaluated even if results already
exist."
  (interactive "P")
  (pcase (org-babel-get-src-block-info 'no-eval)
    (`(,_ ,_ ,arguments ,_ ,_ ,start ,_)
     (save-excursion
       ;; Go to the results, if there aren't any then run the block.
       (goto-char start)
       (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
		      (progn (org-babel-execute-src-block)
			     (org-babel-where-is-src-block-result))))
       (end-of-line)
       (skip-chars-forward " \r\t\n")
       ;; Open the results.
       (if (looking-at org-link-bracket-re) (org-open-at-point)
	 (let ((r (org-babel-format-result (org-babel-read-result)
					   (cdr (assq :sep arguments)))))
	   (pop-to-buffer (get-buffer-create "*Org Babel Results*"))
	   (erase-buffer)
	   (insert r)))
       t))
    (_ nil)))

;;;###autoload
(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body"
  (declare (indent 1) (debug t))
  (let ((tempvar (make-symbol "file")))
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-src-block-regexp nil t)
	   (when (org-babel-active-location-p)
	     (goto-char (match-beginning 0))
	     (let ((full-block (match-string 0))
		   (beg-block (match-beginning 0))
		   (end-block (match-end 0))
		   (lang (match-string 2))
		   (beg-lang (match-beginning 2))
		   (end-lang (match-end 2))
		   (switches (match-string 3))
		   (beg-switches (match-beginning 3))
		   (end-switches (match-end 3))
		   (header-args (match-string 4))
		   (beg-header-args (match-beginning 4))
		   (end-header-args (match-end 4))
		   (body (match-string 5))
		   (beg-body (match-beginning 5))
		   (end-body (match-end 5)))
               ;; Silence byte-compiler in case `body' doesn't use all
               ;; those variables.
               (ignore full-block beg-block end-block lang
                       beg-lang end-lang switches beg-switches
                       end-switches header-args beg-header-args
                       end-header-args body beg-body end-body)
               ,@body
	       (goto-char end-block)))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))

;;;###autoload
(defmacro org-babel-map-inline-src-blocks (file &rest body)
  "Evaluate BODY forms on each inline source block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "src_\\S-" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p ,datum 'inline-src-block)
	       (goto-char (org-element-begin ,datum))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-call-lines (file &rest body)
  "Evaluate BODY forms on each call line in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward "call_\\S-\\|^[ \t]*#\\+CALL:" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p ,datum '(babel-call inline-babel-call))
	       (goto-char (or (org-element-post-affiliated datum)
                              (org-element-begin datum)))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defmacro org-babel-map-executables (file &rest body)
  "Evaluate BODY forms on each active Babel code in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer."
  (declare (indent 1) (debug (form body)))
  (org-with-gensyms (datum end point tempvar to-be-removed visitedp)
    `(let* ((case-fold-search t)
	    (,tempvar ,file)
	    (,visitedp (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (,point (point))
	    ,to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq ,to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward
		 "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)" nil t)
	   (let ((,datum (org-element-context)))
	     (when (org-element-type-p
                    ,datum
                    '(babel-call inline-babel-call inline-src-block src-block))
	       (goto-char (or (org-element-post-affiliated ,datum)
                              (org-element-begin ,datum)))
	       (let ((,end (copy-marker (org-element-end ,datum))))
		 ,@body
		 (goto-char ,end)
		 (set-marker ,end nil))))))
       (unless ,visitedp (kill-buffer ,to-be-removed))
       (goto-char ,point))))

;;;###autoload
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Prefix argument ARG is passed to `org-babel-execute-src-block'.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (if (org-element-type-p
           (org-element-context) '(babel-call inline-babel-call))
          (org-babel-lob-execute-maybe)
        (org-babel-execute-src-block arg)))))

;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree, passing over the prefix argument ARG."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-babel-execute-buffer arg)
      (widen))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info context)
  "Generate a sha1 hash based on the value of INFO.
CONTEXT specifies the context of evaluation.  It can be `:eval',
`:export', `:tangle'.  A nil value means `:eval'."
  (interactive)
  (let ((print-level nil)
	(info (or info (org-babel-get-src-block-info)))
	(context (or context :eval)))
    (setf (nth 2 info)
	  (sort (copy-sequence (nth 2 info))
		(lambda (a b) (string< (car a) (car b)))))
    (let* ((rm (lambda (lst)
		 (dolist (p '("replace" "silent" "none"
			      "discard" "append" "prepend"))
		   (setq lst (remove p lst)))
		 lst))
	   (norm (lambda (arg)
		   (let ((v (if (and (listp (cdr arg)) (null (cddr arg)))
				(copy-sequence (cdr arg))
			      (cdr arg))))
		     (when (and v (not (and (sequencep v)
					  (not (consp v))
					  (= (length v) 0))))
		       (cond
			((and (listp v) ; lists are sorted
			      (member (car arg) '(:result-params)))
			 (sort (funcall rm v) #'string<))
			((and (stringp v) ; strings are sorted
			      (member (car arg) '(:results :exports)))
			 (mapconcat #'identity (sort (funcall rm (split-string v))
						     #'string<) " "))
			(t v))))))
	   ;; expanded body
	   (lang (nth 0 info))
	   (params (nth 2 info))
	   (body (if (org-babel-noweb-p params context)
		     (org-babel-expand-noweb-references info)
		   (nth 1 info)))
	   (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	   (assignments-cmd (intern (concat "org-babel-variable-assignments:"
					    lang)))
	   (expanded
	    (if (fboundp expand-cmd) (funcall expand-cmd body params)
	      (org-babel-expand-body:generic
	       body params (and (fboundp assignments-cmd)
				(funcall assignments-cmd params))))))
      (let* ((it (format "%s-%s"
                         (mapconcat
                          #'identity
                          (delq nil (mapcar (lambda (arg)
                                            (let ((normalized (funcall norm arg)))
                                              (when normalized
                                                (format "%S" normalized))))
                                          (nth 2 info))) ":")
                         expanded))
             (hash (sha1 it)))
        (when (called-interactively-p 'interactive) (message hash))
        hash))))

(defun org-babel-current-result-hash (&optional _info)
  "Return the current in-buffer hash."
  (let ((result (org-babel-where-is-src-block-result nil)))
    (when result
      (org-with-point-at result
	(let ((case-fold-search t)) (looking-at org-babel-result-regexp))
	(match-string-no-properties 1)))))

(defun org-babel-hide-hash ()
  "Hide the hash in the current results line.
Only the initial `org-babel-hash-show' characters of the hash
will remain visible."
  (add-to-invisibility-spec '(org-babel-hide-hash . t))
  (save-excursion
    (when (and (let ((case-fold-search t))
		 (re-search-forward org-babel-result-regexp nil t))
               (match-string 1))
      (let* ((start (match-beginning 1))
             (hide-start (+ org-babel-hash-show start))
             (end (match-end 1))
             (hash (match-string 1))
             ov1 ov2)
        (setq ov1 (make-overlay start hide-start))
        (setq ov2 (make-overlay hide-start end))
        (overlay-put ov2 'invisible 'org-babel-hide-hash)
        (overlay-put ov1 'babel-hash hash)))))

(defun org-babel-hide-all-hashes ()
  "Hide the hash in the current buffer.
Only the initial `org-babel-hash-show' characters of each hash
will remain visible.  This function should be called as part of
the `org-mode-hook'."
  (save-excursion
    (let ((case-fold-search t))
      (while (and (not org-babel-hash-show-time)
		  (re-search-forward org-babel-result-regexp nil t))
	(goto-char (match-beginning 0))
	(org-babel-hide-hash)
	(goto-char (match-end 0))))))
(add-hook 'org-mode-hook #'org-babel-hide-all-hashes)

(defun org-babel-hash-at-point (&optional point)
  "Return the value of the hash at POINT.
\\<org-mode-map>\
The hash is also added as the last element of the kill ring.
This can be called with `\\[org-ctrl-c-ctrl-c]'."
  (interactive)
  (let ((hash (car (delq nil (mapcar
			      (lambda (ol) (overlay-get ol 'babel-hash))
                              (overlays-at (or point (point))))))))
    (when hash (kill-new hash) (message hash))))

(defun org-babel-result-hide-spec ()
  "Hide portions of results lines.
Add `org-babel-hide-result' as an invisibility spec for hiding
portions of results lines."
  (add-to-invisibility-spec '(org-babel-hide-result . t)))
(add-hook 'org-mode-hook #'org-babel-result-hide-spec)

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (let ((case-fold-search t))
      (while (re-search-forward org-babel-result-regexp nil t)
	(save-excursion (goto-char (match-beginning 0))
			(org-babel-hide-result-toggle-maybe))))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

;;;###autoload
(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (and (org-match-line org-babel-result-regexp)
         (progn (org-babel-hide-result-toggle) t))))

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result.
When FORCE is symbol `off', unconditionally display the result.
Otherwise, when FORCE is non-nil, unconditionally hide the result."
  (interactive)
  (save-excursion
    (forward-line 0)
    (let ((case-fold-search t))
      (unless (re-search-forward org-babel-result-regexp nil t)
	(error "Not looking at a result line")))
    (let ((start (progn (forward-line 1) (1- (point))))
	  (end (progn
		 (while (looking-at org-babel-multi-line-header-regexp)
		   (forward-line 1))
		 (goto-char (1- (org-babel-result-end)))
		 (point)))
	  ov)
      (if (memq t (mapcar (lambda (overlay)
			    (eq (overlay-get overlay 'invisible)
				'org-babel-hide-result))
			  (overlays-at start)))
	  (when (or (not force) (eq force 'off))
	    (mapc (lambda (ov)
		    (when (member ov org-babel-hide-result-overlays)
		      (setq org-babel-hide-result-overlays
			    (delq ov org-babel-hide-result-overlays)))
		    (when (eq (overlay-get ov 'invisible)
			      'org-babel-hide-result)
		      (delete-overlay ov)))
		  (overlays-at start)))
	(setq ov (make-overlay start end))
	(overlay-put ov 'invisible 'org-babel-hide-result)
	;; make the block accessible to isearch
	(overlay-put
	 ov 'isearch-open-invisible
	 (lambda (ov)
	   (when (member ov org-babel-hide-result-overlays)
	     (setq org-babel-hide-result-overlays
		   (delq ov org-babel-hide-result-overlays)))
	   (when (eq (overlay-get ov 'invisible)
		     'org-babel-hide-result)
	     (delete-overlay ov))))
	(push ov org-babel-hide-result-overlays)))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-cycle-tab-first-hook #'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (add-hook 'change-major-mode-hook
			       #'org-babel-show-result-all 'append 'local)))

(defun org-babel-params-from-properties (&optional lang no-eval)
  "Retrieve source block parameters specified as properties.

LANG is the language of the source block, as a string.  When
optional argument NO-EVAL is non-nil, do not evaluate Lisp values
in parameters.

Return a list of association lists of source block parameters
specified in the properties of the current outline entry."
  (save-match-data
    (list
     ;; Header arguments specified with the header-args property at
     ;; point of call.
     (org-babel-parse-header-arguments
      (org-entry-get (point) "header-args" 'inherit)
      no-eval)
     ;; Language-specific header arguments at point of call.
     (and lang
	  (org-babel-parse-header-arguments
	   (org-entry-get (point) (concat "header-args:" lang) 'inherit)
	   no-eval)))))

(defun org-babel-balanced-split (string alts)
  "Split STRING on instances of ALTS.
ALTS is a character, or cons of two character options where each
option may be either the numeric code of a single character or
a list of character alternatives.  For example, to split on
balanced instances of \"[ \t]:\", set ALTS to ((32 9) . 58)."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((splitp (lambda (past next)
		    ;; Non-nil when there should be a split after NEXT
		    ;; character. PAST is the character before NEXT.
		    (pcase alts
		      (`(,(and first (pred consp)) . ,(and second (pred consp)))
		       (and (memq past first) (memq next second)))
		      (`(,first . ,(and second (pred consp)))
		       (and (eq past first) (memq next second)))
		      (`(,(and first (pred consp)) . ,second)
		       (and (memq past first) (eq next second)))
		      (`(,first . ,second)
		       (and (eq past first) (eq next second)))
		      ((pred (eq next)) t)
		      (_ nil))))
	  (partial nil)
	  (result nil))
      (while (not (eobp))
        (cond
	 ((funcall splitp (char-before) (char-after))
	  ;; There is a split after point.  If ALTS is two-folds,
	  ;; remove last parsed character as it belongs to ALTS.
	  (when (consp alts) (pop partial))
	  ;; Include elements parsed so far in RESULTS and flush
	  ;; partial parsing.
	  (when partial
	    (push (apply #'string (nreverse partial)) result)
	    (setq partial nil))
	  (forward-char))
	 ((memq (char-after) '(?\( ?\[))
	  ;; Include everything between balanced brackets.
	  (let* ((origin (point))
		 (after (char-after))
		 (openings (list after)))
	    (forward-char)
	    (while (and openings (re-search-forward "[]()]" nil t))
	      (pcase (char-before)
		((and match (or ?\[ ?\()) (push match openings))
		(?\] (when (eq ?\[ (car openings)) (pop openings)))
		(_ (when (eq ?\( (car openings)) (pop openings)))))
	    (if (null openings)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; Un-balanced bracket.  Backtrack.
	      (push after partial)
	      (goto-char (1+ origin)))))
	 ((and (eq ?\" (char-after)) (not (eq ?\\ (char-before))))
	  ;; Include everything from current double quote to next
	  ;; non-escaped double quote.
	  (let ((origin (point)))
	    (if (re-search-forward "[^\\]\"" nil t)
		(setq partial
		      (nconc (nreverse (string-to-list
					(buffer-substring origin (point))))
			     partial))
	      ;; No closing double quote.  Backtrack.
	      (push ?\" partial)
	      (forward-char))))
	 (t (push (char-after) partial)
	    (forward-char))))
      ;; Add pending parsing and return result.
      (when partial (push (apply #'string (nreverse partial)) result))
      (nreverse result))))

(defun org-babel-join-splits-near-ch (ch list)
  "Join strings in LIST where CH is on either end of the strings.
This function will join list elements like \"a=\" \"2\" into \"a=2\"."
  (let ((last= (lambda (str) (= ch (aref str (1- (length str))))))
	(first= (lambda (str) (= ch (aref str 0)))))
    (reverse
     (cl-reduce (lambda (acc el)
		  (let ((head (car acc)))
		    (if (and head (or (funcall last= head) (funcall first= el)))
			(cons (concat head el) (cdr acc))
		      (cons el acc))))
		list :initial-value nil))))

(defun org-babel-parse-header-arguments (string &optional no-eval)
  "Parse header arguments in STRING.
When optional argument NO-EVAL is non-nil, do not evaluate Lisp
in parameters.  Return an alist."
  (when (org-string-nw-p string)
    (org-babel-parse-multiple-vars
     (delq nil
	   (mapcar
	    (lambda (arg)
	      (if (string-match
		   "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
		   arg)
		  (cons (intern (match-string 1 arg))
			(org-babel-read (org-babel-chomp (match-string 2 arg))
					no-eval))
		(cons (intern (org-babel-chomp arg)) nil)))
	    (let ((raw (org-babel-balanced-split string '((32 9) . 58))))
              (cons (car raw)
		    (mapcar (lambda (r) (concat ":" r)) (cdr raw)))))))))

(defun org-babel-parse-multiple-vars (header-arguments)
  "Expand multiple variable assignments behind a single :var keyword.

This allows expression of multiple variables with one :var as
shown below.

#+PROPERTY: var foo=1, bar=2

HEADER-ARGUMENTS is an alist of all the arguments."
  (let (results)
    (mapc (lambda (pair)
	    (if (eq (car pair) :var)
                (or
	         (mapcar (lambda (v) (push (cons :var (org-trim v)) results))
		         (org-babel-join-splits-near-ch
		          61 (org-babel-balanced-split (or (cdr pair) "") 32)))
                 (push `(:var) results))
	      (push pair results)))
	  header-arguments)
    (nreverse results)))

(defun org-babel-process-params (params)
  "Expand variables in PARAMS and add summary parameters."
  (let* ((processed-vars (mapcar (lambda (el)
				   (if (consp el)
				       el
				     (org-babel-ref-parse el)))
				 (org-babel--get-vars params)))
	 (vars-and-names (if (and (assq :colname-names params)
				  (assq :rowname-names params))
			     (list processed-vars)
			   (org-babel-disassemble-tables
			    processed-vars
			    (cdr (assq :hlines params))
			    (cdr (assq :colnames params))
			    (cdr (assq :rownames params)))))
	 (raw-result (or (cdr (assq :results params)) ""))
	 (result-params (delete-dups
			 (append
			  (split-string (if (stringp raw-result)
					    raw-result
                                          ;; FIXME: Arbitrary code evaluation.
					  (eval raw-result t)))
			  (cdr (assq :result-params params))))))
    (append
     (mapcar (lambda (var) (cons :var var)) (car vars-and-names))
     (list
      (cons :colname-names (or (cdr (assq :colname-names params))
			       (cadr  vars-and-names)))
      (cons :rowname-names (or (cdr (assq :rowname-names params))
			       (cl-caddr vars-and-names)))
      (cons :result-params result-params)
      (cons :result-type  (cond ((member "output" result-params) 'output)
				((member "value" result-params) 'value)
				(t 'value))))
     (cl-remove-if
      (lambda (x) (memq (car x) '(:colname-names :rowname-names :result-params
					         :result-type :var)))
      params))))

;; row and column names
(defun org-babel-del-hlines (table)
  "Remove all `hline's from TABLE."
  (remq 'hline table))

(defun org-babel-get-colnames (table)
  "Return the column names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names."
  ;; Skip over leading hlines.
  (while (eq 'hline (car table)) (pop table))
  (if (eq 'hline (nth 1 table))
      (cons (cddr table) (car table))
    (cons (cdr table) (car table))))

(defun org-babel-get-rownames (table)
  "Return the row names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
rownames, and the `cdr' of which contains a list of the rownames.
Note: this function removes any hlines in TABLE."
  (let* ((table (org-babel-del-hlines table))
	 (rownames (funcall (lambda ()
			      (let ((tp table))
				(mapcar
				 (lambda (_row)
				   (prog1
				       (pop (car tp))
				     (setq tp (cdr tp))))
				 table))))))
    (cons table rownames)))

(defun org-babel-put-colnames (table colnames)
  "Add COLNAMES to TABLE if they exist."
  (if colnames (apply 'list colnames 'hline table) table))

(defun org-babel-put-rownames (table rownames)
  "Add ROWNAMES to TABLE if they exist."
  (if rownames
      (mapcar (lambda (row)
                (if (listp row)
                    (cons (or (pop rownames) "") row)
                  row))
	      table)
    table))

(defun org-babel-pick-name (names selector)
  "Select one out of an alist of row or column names.
SELECTOR can be either a list of names in which case those names
will be returned directly, or an index into the list NAMES in
which case the indexed names will be return."
  (if (listp selector)
      selector
    (when names
      (if (and selector (symbolp selector) (not (equal t selector)))
	  (cdr (assoc selector names))
	(if (integerp selector)
	    (nth (- selector 1) names)
	  (cdr (car (last names))))))))

(defun org-babel-disassemble-tables (vars hlines colnames rownames)
  "Parse tables for further processing.
Process the variables in VARS according to the HLINES,
ROWNAMES and COLNAMES header arguments.  Return a list consisting
of the vars, cnames and rnames."
  (let (cnames rnames)
    (list
     (mapcar
      (lambda (var)
        (when (proper-list-p (cdr var))
          (when (and (not (equal colnames "no"))
                     ;; Compatibility note: avoid `length>', which
                     ;; isn't available until Emacs 28.
                     (or colnames
                         ;; :colnames nil (default)
                         ;; Auto-assign column names when the table
                         ;; has hline as the second line after
                         ;; non-hline row.
                         (and (> (length (cdr var)) 1)
                              (not (eq (car (cdr var)) 'hline)) ; first row
                              (eq (nth 1 (cdr var)) 'hline) ; second row
                              (not (member 'hline (cddr (cdr var)))) ; other rows
                              )))
            (let ((both (org-babel-get-colnames (cdr var))))
              (setq cnames (cons (cons (car var) (cdr both))
                                 cnames))
              (setq var (cons (car var) (car both)))))
          (when (and rownames (not (equal rownames "no")))
            (let ((both (org-babel-get-rownames (cdr var))))
              (setq rnames (cons (cons (car var) (cdr both))
                                 rnames))
              (setq var (cons (car var) (car both)))))
          (when (and hlines (not (equal hlines "yes")))
            (setq var (cons (car var) (org-babel-del-hlines (cdr var))))))
        var)
      vars)
     (reverse cnames) (reverse rnames))))

(defun org-babel-reassemble-table (table colnames rownames)
  "Add column and row names to a table.
Given a TABLE and set of COLNAMES and ROWNAMES add the names
to the table for reinsertion to `org-mode'."
  (if (listp table)
      (let ((table (if (and rownames (= (length table) (length rownames)))
                       (org-babel-put-rownames table rownames) table)))
        (if (and colnames (listp (car table)) (= (length (car table))
                                                 (length colnames)))
            (org-babel-put-colnames table colnames) table))
    table))

(defun org-babel-where-is-src-block-head (&optional src-block)
  "Find where the current source block begins.

If optional argument SRC-BLOCK is `src-block' type element, find
its current beginning instead.

Return the point at the beginning of the current source block.
Specifically at the beginning of the #+BEGIN_SRC line.  Also set
`match-data' relatively to `org-babel-src-block-regexp', which see.
If the point is not on a source block or within blank lines after an
src block, then return nil."
  (let ((element (or src-block (org-element-at-point))))
    (when (org-element-type-p element 'src-block)
      (let ((end (org-element-end element)))
	(org-with-wide-buffer
	 ;; Ensure point is not on a blank line after the block.
	 (forward-line 0)
	 (skip-chars-forward " \r\t\n" end)
	 (when (< (point) end)
	   (prog1 (goto-char (org-element-post-affiliated element))
	     (looking-at org-babel-src-block-regexp))))))))

;;;###autoload
(defun org-babel-goto-src-block-head ()
  "Go to the beginning of the current code block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (if head (goto-char head) (error "Not currently in a code block"))))

;;;###autoload
(defun org-babel-goto-named-src-block (name)
  "Go to a source-code block with NAME."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 (all-block-names (org-babel-src-block-names)))
     (list (completing-read
	    "source-block name: " all-block-names nil t
	    (let* ((context (org-element-context))
		   (type (org-element-type context))
		   (noweb-ref
		    (and (memq type '(inline-src-block src-block))
			 (org-in-regexp (org-babel-noweb-wrap)))))
	      (cond
	       (noweb-ref
		(buffer-substring
		 (+ (car noweb-ref) (length org-babel-noweb-wrap-start))
		 (- (cdr noweb-ref) (length org-babel-noweb-wrap-end))))
	       ((memq type '(babel-call inline-babel-call)) ;#+CALL:
		(org-element-property :call context))
	       ((car (org-element-property :results context))) ;#+RESULTS:
	       ((let ((symbol (thing-at-point 'symbol))) ;Symbol.
		  (and symbol
		       (member-ignore-case symbol all-block-names)
		       symbol)))
	       (t "")))))))
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; Taken from `org-open-at-point'.
        (progn (org-mark-ring-push) (goto-char point) (org-fold-show-context))
      (message "source-code block `%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists.  Set match data according
to `org-babel-named-src-block-regexp'."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (org-babel-named-src-block-regexp-for-name name)))
      (or (and (looking-at regexp)
	       (progn (goto-char (match-beginning 1))
		      (line-beginning-position)))
	  (ignore-errors (org-next-block 1 nil regexp))))))

(defun org-babel-src-block-names (&optional file)
  "Return the names of source blocks in FILE or the current buffer."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((regexp "^[ \t]*#\\+begin_src ")
	    (case-fold-search t)
	    (names nil))
	(while (re-search-forward regexp nil t)
	  (let ((element (org-element-at-point)))
	    (when (org-element-type-p element 'src-block)
	      (let ((name (org-element-property :name element)))
		(when name (push name names))))))
	names))))

;;;###autoload
(defun org-babel-goto-named-result (name)
  "Go to a result with NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Source-block name: "
			    (org-babel-result-names) nil t))))
  (let ((point (org-babel-find-named-result name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-fold-show-context))
      (message "result `%s' not found in this buffer" name))))

(defun org-babel-find-named-result (name)
  "Find a named result.
Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (format "^[ \t]*#\\+%s.*?:[ \t]*%s[ \t]*$"
		      org-babel-results-keyword
		      (regexp-quote name))))
      (catch :found
	(while (re-search-forward re nil t)
	  (let ((element (org-element-at-point)))
	    (when (or (org-element-type-p element 'keyword)
		      (< (point)
			 (org-element-post-affiliated element)))
	      (throw :found (line-beginning-position)))))))))

(defun org-babel-result-names (&optional file)
  "Return the names of results in FILE or the current buffer."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((case-fold-search t) names)
      (while (re-search-forward org-babel-result-w-name-regexp nil t)
	(setq names (cons (match-string-no-properties 9) names)))
      names))))

;;;###autoload
(defun org-babel-next-src-block (&optional arg)
  "Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "p")
  (org-next-block arg nil org-babel-src-block-regexp))

;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "p")
  (org-previous-block arg org-babel-src-block-regexp))

(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-mark-block ()
  "Mark current source block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (when head
      (save-excursion
        (goto-char head)
        (looking-at org-babel-src-block-regexp))
      (push-mark (match-end 5) nil t)
      (goto-char (match-beginning 5)))))

(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in an active region or at point.

With prefix argument ARG, also create a new heading at point.

When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated.

When called within blank lines after a code block, create a new code
block of the same language as the previous."
  (interactive "P")
  (let* ((info (org-babel-get-src-block-info 'no-eval))
	 (start (org-babel-where-is-src-block-head))
         ;; `start' will be nil when within space lines after src block.
	 (block (and start (match-string 0)))
         (body-beg (and start (match-beginning 5)))
         (body-end (and start (match-end 5)))
	 (stars (concat (make-string (or (org-current-level) 1) ?*) " "))
	 (upper-case-p (and block
			    (let (case-fold-search)
			      (string-match-p "#\\+BEGIN_SRC" block)))))
    (if (and info start) ;; At src block, but not within blank lines after it.
        (let* ((copy (org-element-copy (org-element-at-point)))
               (before (org-element-begin copy))
               (beyond (org-element-end copy))
               (parts
                (if (org-region-active-p)
                    (list body-beg (region-beginning) (region-end) body-end)
                  (list body-beg (point) body-end)))
               (pads ;; To calculate left-side white-space padding.
                (if (org-region-active-p)
                    (list (region-beginning) (region-end))
                  (list (point))))
               (n (- (length parts) 2)) ;; 1 or 2 parts in `dolist' below.
               ;; `post-blank' caches the property before setting it to 0.
               (post-blank (org-element-property :post-blank copy)))
          ;; Point or region are within body when parts is in increasing order.
          (unless (apply #'<= parts)
            (user-error "Select within the source block body to split it"))
          (setq parts (mapcar (lambda (p) (buffer-substring (car p) (cdr p)))
                              (seq-mapn #'cons parts (cdr parts))))
          ;; Map positions to columns for white-space padding.
          (setq pads (mapcar (lambda (p) (save-excursion
                                           (goto-char p)
                                           (current-column)))
                             pads))
          (push 0 pads) ;; The 1st part never requires white-space padding.
          (setq parts (mapcar (lambda (p) (string-join
                                           (list (make-string (car p) ?\s)
                                                 (cdr p))))
                              (seq-mapn #'cons pads parts)))
          (delete-region before beyond)
          ;; Set `:post-blank' to 0.  We take care of spacing between blocks.
          (org-element-put-property copy :post-blank 0)
          (org-element-put-property copy :value (car parts))
          (insert (org-element-interpret-data copy))
          ;; `org-indent-block' may see another `org-element' (e.g. paragraph)
          ;; immediately after the block.  Ensure to indent the inserted block
          ;; and move point to its end.
          (org-babel-previous-src-block 1)
          (org-indent-block)
          (goto-char (org-element-end (org-element-at-point)))
          (org-element-put-property copy :caption nil)
          (org-element-put-property copy :name nil)
          ;; Insert the 2nd block, and the 3rd block when region is active.
          (dolist (part (cdr parts))
            (org-element-put-property copy :value part)
            (insert (if arg (concat stars "\n") "\n"))
            (cl-decf n)
            (when (= n 0)
              ;; Use `post-blank' to reset the property of the last block.
              (org-element-put-property copy :post-blank post-blank))
            (insert (org-element-interpret-data copy))
            ;; Ensure to indent the inserted block and move point to its end.
            (org-babel-previous-src-block 1)
            (org-indent-block)
            (goto-char (org-element-end (org-element-at-point))))
          ;; Leave point at the last inserted block.
          (goto-char (org-babel-previous-src-block 1)))
      (let ((start (point))
	    (lang (or (car info) ; Reuse language from previous block.
                      (completing-read
		       "Lang: "
		       (mapcar #'symbol-name
			       (delete-dups
			        (append (mapcar #'car org-babel-load-languages)
				        (mapcar (lambda (el) (intern (car el)))
					        org-src-lang-modes)))))))
	    (body (delete-and-extract-region
		   (if (org-region-active-p) (mark) (point)) (point))))
	(insert (concat (if (looking-at "^") "" "\n")
			(if arg (concat stars "\n") "")
			(if upper-case-p "#+BEGIN_SRC " "#+begin_src ")
			lang "\n" body
			(if (or (= (length body) 0)
				(string-suffix-p "\r" body)
				(string-suffix-p "\n" body))
			    ""
			  "\n")
			(if upper-case-p "#+END_SRC\n" "#+end_src\n")))
	(goto-char start)
	(move-end-of-line 1)))))

(defun org-babel--insert-results-keyword (name hash)
  "Insert RESULTS keyword with NAME value at point.
If NAME is nil, results are anonymous.  HASH is a string used as
the results hash, or nil.  Leave point before the keyword."
  (save-excursion (insert "\n"))	;open line to indent.
  (org-indent-line)
  (delete-char 1)
  (insert (concat "#+" org-babel-results-keyword
		  (cond ((not hash) nil)
			(org-babel-hash-show-time
			 (format "[%s %s]"
				 (format-time-string "(%F %T)")
				 hash))
			(t (format "[%s]" hash)))
		  ":"
		  (when name (concat " " name))
		  "\n"))
  ;; Make sure results are going to be followed by at least one blank
  ;; line so they do not get merged with the next element, e.g.,
  ;;
  ;;   #+results:
  ;;   : 1
  ;;
  ;;   : fixed-width area, unrelated to the above.
  (unless (looking-at "^[ \t]*$") (save-excursion (insert "\n")))
  (forward-line -1)
  (when hash (org-babel-hide-hash)))

(defun org-babel--clear-results-maybe (hash)
  "Clear results when hash doesn't match HASH.

When results hash does not match HASH, remove RESULTS keyword at
point, along with related contents.  Do nothing if HASH is nil.

Return a non-nil value if results were cleared.  In this case,
leave point where new results should be inserted."
  (when hash
    (let ((case-fold-search t)) (looking-at org-babel-result-regexp))
    (unless (string= (match-string 1) hash)
      (let* ((e (org-element-at-point))
	     (post (copy-marker (org-element-post-affiliated e))))
	;; Delete contents.
	(delete-region post
		       (save-excursion
			 (goto-char (org-element-end e))
			 (skip-chars-backward " \t\n")
			 (line-beginning-position 2)))
	;; Delete RESULT keyword.  However, if RESULTS keyword is
	;; orphaned, ignore this part.  The deletion above already
	;; took care of it.
	(unless (= (point) post)
	  (delete-region (line-beginning-position)
			 (line-beginning-position 2)))
	(goto-char post)
	(set-marker post nil)
	t))))

(defun org-babel-where-is-src-block-result (&optional insert _info hash)
  "Find where the current source block results begin.

Return the point at the beginning of the result of the current
source block, specifically at the beginning of the results line.

If no result exists for this block return nil, unless optional
argument INSERT is non-nil.  In this case, create a results line
following the source block and return the position at its
beginning.  In the case of inline code, remove the results part
instead.

If optional argument HASH is a string, remove contents related to
RESULTS keyword if its hash is different.  Then update the latter
to HASH."
  (let ((context (org-element-context)))
    (catch :found
      (org-with-wide-buffer
       (pcase (org-element-type context)
	 ((or `inline-babel-call `inline-src-block)
	  ;; Results for inline objects are located right after them.
	  ;; There is no RESULTS line to insert either.
	  (let ((limit (or (org-element-contents-end (org-element-parent context))
                           (org-element-end (org-element-parent context)))))
	    (goto-char (org-element-end context))
	    (skip-chars-forward " \t\n" limit)
	    (throw :found
		   (and
		    (< (point) limit)
		    (let ((result (org-element-context)))
		      (and (org-element-type-p result 'macro)
			   (string= (org-element-property :key result)
				    "results")
			   (if (not insert) (point)
			     (delete-region
			      (point)
			      (progn
				(goto-char (org-element-end result))
				(skip-chars-backward " \t")
				(point)))
			     (point))))))))
	 ((or `babel-call `src-block)
	  (let* ((name (org-element-property :name context))
		 (named-results (and name (org-babel-find-named-result name))))
	    (goto-char (or named-results (org-element-end context)))
	    (cond
	     ;; Existing results named after the current source.
	     (named-results
	      (when (org-babel--clear-results-maybe hash)
		(org-babel--insert-results-keyword name hash))
	      (throw :found (point)))
	     ;; Named results expect but none to be found.
	     (name)
	     ;; No possible anonymous results at the very end of
	     ;; buffer or outside CONTEXT parent.
	     ((eq (point)
		  (or (pcase (org-element-type (org-element-parent context))
                        ((or `section `org-data)
                         (org-element-end (org-element-parent context)))
                        (_ (org-element-contents-end
                            (org-element-parent context))))
		      (point-max))))
	     ;; Check if next element is an anonymous result below
	     ;; the current block.
	     ((let* ((next (org-element-at-point))
		     (end (save-excursion
			    (goto-char
			     (org-element-post-affiliated next))
			    (line-end-position)))
		     (empty-result-re (concat org-babel-result-regexp "$"))
		     (case-fold-search t))
		(re-search-forward empty-result-re end t))
	      (forward-line 0)
	      (when (org-babel--clear-results-maybe hash)
		(org-babel--insert-results-keyword nil hash))
	      (throw :found (point))))))
	 ;; Ignore other elements.
	 (_ (throw :found nil))))
      ;; No result found.  Insert a RESULTS keyword below element, if
      ;; appropriate.  In this case, ensure there is an empty line
      ;; after the previous element.
      (when insert
	(save-excursion
	  (goto-char (min (org-element-end context) (point-max)))
	  (skip-chars-backward " \t\n")
	  (forward-line)
	  (unless (bolp) (insert "\n"))
	  (insert "\n")
	  (org-babel--insert-results-keyword
	   (org-element-property :name context) hash)
	  (point))))))

(defun org-babel-read-element (element)
  "Read ELEMENT into emacs-lisp.
Return nil if ELEMENT cannot be read."
  (org-with-wide-buffer
   (goto-char (org-element-post-affiliated element))
   (pcase (org-element-type element)
     (`fixed-width
      (let ((v (org-trim (org-element-property :value element))))
	(or (org-babel--string-to-number v) v)))
     (`table (org-babel-read-table))
     (`plain-list (org-babel-read-list))
     ((or `example-block `src-block)
      (let ((v (org-element-property :value element)))
	(if (org-src-preserve-indentation-p element) v
	  (org-remove-indentation v))))
     (`export-block
      (org-remove-indentation (org-element-property :value element)))
     (`paragraph
      ;; Treat paragraphs containing a single link specially.
      (skip-chars-forward " \t")
      (if (and (looking-at org-link-bracket-re)
	       (save-excursion
		 (goto-char (match-end 0))
		 (skip-chars-forward " \r\t\n")
		 (<= (org-element-end element)
		     (point))))
	  (org-babel-read-link)
	(buffer-substring-no-properties
	 (org-element-contents-begin element)
	 (org-element-contents-end element))))
     ((or `center-block `quote-block `verse-block `special-block)
      (org-remove-indentation
       (buffer-substring-no-properties
	(org-element-contents-begin element)
	(org-element-contents-end element))))
     (_ nil))))

(defun org-babel-read-result ()
  "Read the result at point into emacs-lisp."
  (and (not (save-excursion
	    (forward-line 0)
	    (looking-at-p "[ \t]*$")))
       (org-babel-read-element (org-element-at-point))))

(defun org-babel-read-table ()
  "Read the table at point into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar (lambda (el) (org-babel-read el 'inhibit-lisp-eval)) row)))
          (org-table-to-lisp)))

(defun org-babel-read-list ()
  "Read the list at point into emacs-lisp.

Return the list of strings representing top level items:

   (item1 item2 ...)

Only consider top level items.  See Info node
`(org)Environment of a Code Block'."
  (mapcar (lambda (el) (org-babel-read (car el) 'inhibit-lisp-eval))
	  (cdr (org-list-to-lisp))))

(defvar org-link-types-re)
(defun org-babel-read-link ()
  "Read the link at point into emacs-lisp.
If the path of the link is a file path it is expanded using
`expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-link-bracket-re)
                   (org-no-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(defun org-babel-format-result (result &optional sep)
  "Format RESULT for writing to file.
When RESULT is a list, write it as a table, use tab or SEP as column
separator."
  (let ((echo-res (lambda (r) (if (stringp r) r (format "%S" r)))))
    (if (listp result)
	;; table result
	(orgtbl-to-generic
	 result (list :sep (or sep "\t") :fmt echo-res))
      ;; scalar result
      (funcall echo-res result))))

(defun org-babel-insert-result (result &optional result-params info hash lang exec-time)
  "Insert RESULT into the current buffer.

By default RESULT is inserted after the end of the current source
block.  The RESULT of an inline source block usually will be
wrapped inside a `results' macro and placed on the same line as
the inline source block.  The macro is stripped upon export.
Multiline and non-scalar RESULTS from inline source blocks are
not allowed.  When EXEC-TIME is provided it may be included in a
generated message.  With optional argument RESULT-PARAMS controls
insertion of results in the Org mode file.  RESULT-PARAMS is a list
that can contain the following values:

replace - (default option) insert results after the source block
          or inline source block replacing any previously
          inserted results.

silent -- no results are inserted into the Org buffer but
          the results are echoed to the minibuffer and are
          ingested by Emacs (a potentially time consuming
          process).

none ---- no results are inserted into the Org buffer nor
          echoed to the minibuffer.  They are not processed into
          Emacs-lisp objects at all.

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org file syntax.

list ---- the results are interpreted as an Org list.

raw ----- results are added directly to the Org file.  This is
          a good option if you code block will output Org
          formatted text.

drawer -- results are added directly to the Org file as with
          \"raw\", but are wrapped in a RESULTS drawer or results
          macro, allowing them to later be replaced or removed
          automatically.

org ----- results are added inside of a \"src_org{}\" or \"#+BEGIN_SRC
          org\" block depending on whether the current source block is
          inline or not.  They are not comma-escaped when inserted,
          but Org syntax here will be discarded when exporting the
          file.

html ---- results are added inside of a #+BEGIN_EXPORT HTML block
          or html export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output html formatted text.

latex --- results are added inside of a #+BEGIN_EXPORT LATEX
          block or latex export snippet depending on whether the
          current source block is inline or not.  This is a good
          option if your code block will output latex formatted
          text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a source block with the source-code language
          set appropriately.  Also, source block inlining is
          preserved in this case.  Note this relies on the
          optional LANG argument.

list ---- the results are rendered as a list.  This option not
          allowed for inline source blocks.

table --- the results are rendered as a table.  This option not
          allowed for inline source blocks.

INFO is the src block info, as returned by
`org-babel-get-src-block-info' (which see).  Some values from its
PARAMETERS part (header argument alist) can affect the inserted
result:

:file-desc - when RESULT-PARAMS contains \"file\", use it as
             description of the inserted link.

:wrap        the effect is similar to `latex' in RESULT-PARAMS but
             using the argument supplied to specify the export block
             or snippet type."
  (cond ((stringp result)
	 (setq result (substring-no-properties result))
	 (when (member "file" result-params)
	   (setq result
                 (org-babel-result-to-file
		  result
		  (org-babel--file-desc (nth 2 info) result)
                  'attachment))))
	((listp result))
	(t (setq result (format "%S" result))))

  (if (and result-params (member "silent" result-params))
      (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	     result)
    (let ((inline (let ((context (org-element-context)))
		    (and (org-element-type-p
                          context '(inline-babel-call inline-src-block))
			 context))))
      (when inline
	(let ((warning
	       (or (and (member "table" result-params) "`:results table'")
                   (and (member "drawer" result-params) "`:results drawer'")
		   (and result (listp result) "list result")
		   (and result (string-match-p "\n." result) "multiline result")
		   (and (member "list" result-params) "`:results list'"))))
	  (when warning
	    (user-error "Inline error: %s cannot be used" warning))))
      (save-excursion
	(let* ((visible-beg (point-min-marker))
	       (visible-end (copy-marker (point-max) t))
	       (existing-result (org-babel-where-is-src-block-result t nil hash))
	       (results-switches (cdr (assq :results_switches (nth 2 info))))
	       ;; When results exist outside of the current visible
	       ;; region of the buffer, be sure to widen buffer to
	       ;; update them.
	       (outside-scope (and existing-result
				   (buffer-narrowed-p)
				   (or (> visible-beg existing-result)
				       (<= visible-end existing-result))))
	       beg end indent)
	  ;; Ensure non-inline results end in a newline.
	  (when (and (org-string-nw-p result)
		     (not inline)
		     (not (string-equal (substring result -1) "\n")))
	    (setq result (concat result "\n")))
	  (unwind-protect
	      (progn
		(when outside-scope (widen))
		(if existing-result (goto-char existing-result)
		  (goto-char (org-element-end inline))
		  (skip-chars-backward " \t"))
		(unless inline
		  (setq indent (current-indentation))
		  (forward-line 1))
		(setq beg (point))
		(cond
		 (inline
		   ;; Make sure new results are separated from the
		   ;; source code by one space.
		   (unless existing-result
		     (insert " ")
		     (setq beg (point))))
		 ((member "replace" result-params)
		  (delete-region (point) (org-babel-result-end)))
		 ((member "append" result-params)
		  (goto-char (org-babel-result-end)) (setq beg (point-marker)))
		 ;; ((member "prepend" result-params)) ; already there
                 )
		(setq results-switches
		      (if results-switches (concat " " results-switches) ""))
		(let ((wrap
		       (lambda (start finish &optional no-escape no-newlines
				      inline-start inline-finish)
			 (when inline
			   (setq start inline-start)
			   (setq finish inline-finish)
			   (setq no-newlines t))
			 (let ((before-finish (copy-marker end)))
			   (goto-char end)
			   (insert (concat finish (unless no-newlines "\n")))
			   (goto-char beg)
			   (insert (concat start (unless no-newlines "\n")))
			   (unless no-escape
			     (org-escape-code-in-region
			      (min (point) before-finish) before-finish))
			   (goto-char end))))
		      (tabulablep
		       (lambda (r)
			 ;; Non-nil when result R can be turned into
			 ;; a table.
                         (and (proper-list-p r)
			      (cl-every
                               (lambda (e) (or (atom e) (proper-list-p e)))
			       result)))))
		  ;; insert results based on type
		  (cond
		   ;; Do nothing for an empty result.
		   ((null result))
		   ;; Insert a list if preferred.
		   ((member "list" result-params)
		    (insert
		     (org-trim
		      (org-list-to-org
                       ;; We arbitrarily choose to format non-strings
                       ;; as %S.
		       (cons 'unordered
			     (mapcar
			      (lambda (e)
                                (cond
                                 ((stringp e) (list e))
                                 ((listp e)
                                  (mapcar
                                   (lambda (x)
                                     (if (stringp x) x (format "%S" x)))
                                   e))
                                 (t (list (format "%S" e)))))
			      (if (listp result) result
				(split-string result "\n" t))))
		       '(:splicep nil :istart "- " :iend "\n")))
		     "\n"))
		   ;; Try hard to print RESULT as a table.  Give up if
		   ;; it contains an improper list.
		   ((funcall tabulablep result)
		    (goto-char beg)
		    (insert (concat (orgtbl-to-orgtbl
				     (if (cl-every
					  (lambda (e)
					    (or (eq e 'hline) (listp e)))
					  result)
					 result
				       (list result))
				     nil)
				    "\n"))
		    (goto-char beg)
		    (when (org-at-table-p) (org-table-align))
		    (goto-char (org-table-end)))
		   ;; Print verbatim a list that cannot be turned into
		   ;; a table.
		   ((listp result) (insert (format "%s\n" result)))
		   ((member "file" result-params)
		    (when inline
		      (setq result (org-macro-escape-arguments result)))
		    (insert result))
		   ((and inline (not (member "raw" result-params)))
		    (insert (org-macro-escape-arguments
			     (org-babel-chomp result "\n"))))
		   (t (goto-char beg) (insert result)))
		  (setq end (copy-marker (point) t))
		  ;; Possibly wrap result.
		  (cond
		   ((assq :wrap (nth 2 info))
		    (let* ((full (or (cdr (assq :wrap (nth 2 info))) "results"))
			   (split (split-string full))
			   (type (car split))
			   (opening-line (concat "#+begin_" full))
			   (closing-line (concat "#+end_" type)))
		      (cond
		       ;; Escape contents from "export" wrap.  Wrap
		       ;; inline results within an export snippet with
		       ;; appropriate value.
		       ((org-string-equal-ignore-case type "export")
			(let ((backend (pcase split
					 (`(,_) "none")
					 (`(,_ ,b . ,_) b))))
			  (funcall wrap
				   opening-line closing-line
				   nil nil
				   (format "{{{results(@@%s:"
					   backend) "@@)}}}")))
		       ;; Escape contents from "example" wrap.  Mark
		       ;; inline results as verbatim.
		       ((org-string-equal-ignore-case type "example")
			(funcall wrap
				 opening-line closing-line
				 nil nil
				 "{{{results(=" "=)}}}"))
		       ;; Escape contents from "src" wrap.  Mark
		       ;; inline results as inline source code.
		       ((org-string-equal-ignore-case type "src")
			(let ((inline-open
			       (pcase split
				 (`(,_)
				  "{{{results(src_none{")
				 (`(,_ ,language)
				  (format "{{{results(src_%s{" language))
				 (`(,_ ,language . ,rest)
				  (let ((r (mapconcat #'identity rest " ")))
				    (format "{{{results(src_%s[%s]{"
					    language r))))))
			  (funcall wrap
				   opening-line closing-line
				   nil nil
				   inline-open "})}}}")))
		       ;; Do not escape contents in non-verbatim
		       ;; blocks.  Return plain inline results.
		       (t
			(funcall wrap
				 opening-line closing-line
				 t nil
				 "{{{results(" ")}}}")))))
		   ((member "html" result-params)
		    (funcall wrap "#+begin_export html" "#+end_export" nil nil
			     "{{{results(@@html:" "@@)}}}"))
		   ((member "latex" result-params)
		    (funcall wrap "#+begin_export latex" "#+end_export" nil nil
			     "{{{results(@@latex:" "@@)}}}"))
		   ((member "org" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap "#+begin_src org" "#+end_src" nil nil
			     "{{{results(src_org{" "})}}}"))
		   ((member "code" result-params)
		    (let ((lang (or lang "none")))
		      (funcall wrap (format "#+begin_src %s%s" lang results-switches)
			       "#+end_src" nil nil
			       (format "{{{results(src_%s[%s]{" lang results-switches)
			       "})}}}")))
		   ((member "raw" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle)))
		   ((or (member "drawer" result-params)
			;; Stay backward compatible with <7.9.2
			(member "wrap" result-params))
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap ":results:" ":end:" 'no-escape nil
			     "{{{results(" ")}}}"))
		   ((and inline (member "file" result-params))
		    (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
		   ((and (not (funcall tabulablep result))
			 (not (member "file" result-params)))
		    (let ((org-babel-inline-result-wrap
			   ;; Hard code {{{results(...)}}} on top of
			   ;; customization.
			   (format "{{{results(%s)}}}"
				   org-babel-inline-result-wrap)))
		      (org-babel-examplify-region
		       beg end results-switches inline)))))
		;; Possibly indent results in par with #+results line.
		(when (and (not inline) (numberp indent) (> indent 0)
			   ;; In this case `table-align' does the work
			   ;; for us.
			   (not (and (listp result)
				     (member "append" result-params))))
		  (indent-rigidly beg end indent))
                (unless noninteractive
                  (let ((time-info
                         ;; Only show the time when something other than
                         ;; 0s will be shown, i.e. check if the time is at
                         ;; least half of the displayed precision.
                         (if (and exec-time (> (float-time exec-time) 0.05))
                             (format " (took %.1fs)" (float-time exec-time))
                           "")))
                    (if (null result)
                        (if (member "value" result-params)
                            (message "Code block returned no value%s." time-info)
                          (message "Code block produced no output%s." time-info))
                      (message "Code block evaluation complete%s." time-info)))))
	    (when end (set-marker end nil))
	    (when outside-scope (narrow-to-region visible-beg visible-end))
	    (set-marker visible-beg nil)
	    (set-marker visible-end nil)))))))

(defun org-babel-remove-result (&optional info keep-keyword)
  "Remove the result of the current source block.
INFO argument is currently ignored.
When KEEP-KEYWORD is non-nil, keep the #+RESULT keyword and just remove
the rest of the result."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info))
	(case-fold-search t))
    (when location
      (save-excursion
        (goto-char location)
	(when (looking-at org-babel-result-regexp)
	  (delete-region
	   (if keep-keyword (line-beginning-position 2)
	     (save-excursion
	       (skip-chars-backward " \r\t\n")
	       (line-beginning-position 2)))
	   (progn (forward-line) (org-babel-result-end))))))))

(defun org-babel-remove-inline-result (&optional datum)
  "Remove the result of DATUM or the current inline-src-block or babel call.
The result must be wrapped in a `results' macro to be removed.
Leading white space is trimmed."
  (interactive)
  (let* ((el (or datum (org-element-context))))
    (when (org-element-type-p el '(inline-src-block inline-babel-call))
      (org-with-wide-buffer
       (goto-char (org-element-end el))
       (skip-chars-backward " \t")
       (let ((result (save-excursion
		       (skip-chars-forward
			" \t\n"
			(org-element-contents-end
			 (org-element-parent el)))
		       (org-element-context))))
	 (when (and (org-element-type-p result 'macro)
		    (string= (org-element-property :key result) "results"))
	   (delete-region		; And leading whitespace.
	    (point)
	    (progn (goto-char (org-element-end result))
		   (skip-chars-backward " \t\n")
		   (point)))))))))

(defun org-babel-remove-result-one-or-many (arg)
  "Remove the result of the current source block.
If called with prefix argument ARG, remove all result blocks in the
buffer."
  (interactive "P")
  (if arg
      (progn
        (org-babel-map-src-blocks nil (org-babel-remove-result))
        (org-babel-map-call-lines nil (org-babel-remove-result)))
    (org-babel-remove-result)))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results."
  (cond ((looking-at-p "^[ \t]*$") (point)) ;no result
	((looking-at-p (format "^[ \t]*%s[ \t]*$" org-link-bracket-re))
	 (line-beginning-position 2))
	(t
	 (let ((element (org-element-at-point)))
	   (if (org-element-type-p
                element
		;; Possible results types.
                '(drawer example-block export-block fixed-width
                         special-block src-block item plain-list table
                         latex-environment))
	       (save-excursion
		 (goto-char (min (point-max) ;for narrowed buffers
				 (org-element-end element)))
		 (skip-chars-backward " \r\t\n")
		 (line-beginning-position 2))
	     (point))))))

(defun org-babel-result-to-file (result &optional description type)
  "Convert RESULT into an Org link with optional DESCRIPTION.
If the `default-directory' is different from the containing
file's directory then expand relative links.

If the optional TYPE is passed as `attachment' and the path is a
descendant of the DEFAULT-DIRECTORY, the generated link will be
specified as an \"attachment:\" style link."
  (when (stringp result)
    (let* ((result-file-name (expand-file-name result))
           (base-file-name (buffer-file-name (buffer-base-buffer)))
           (base-directory (and base-file-name
                                (file-name-directory base-file-name)))
           (same-directory?
	    (and base-file-name
	         (not (string= (expand-file-name default-directory)
			       (expand-file-name
			        base-directory)))))
           (request-attachment (eq type 'attachment))
           (attach-dir (let* ((default-directory base-directory)
                              (dir (org-attach-dir nil t)))
                         (when dir
                           (expand-file-name dir))))
           (in-attach-dir (and request-attachment
                               attach-dir
                               (string-prefix-p
                                attach-dir
                                result-file-name))))
      (format "[[%s:%s]%s]"
              (pcase type
                ((and 'attachment (guard in-attach-dir)) "attachment")
                (_ "file"))
              (if (and request-attachment in-attach-dir)
                  (file-relative-name
                   result-file-name
                   (file-name-as-directory attach-dir))
	        (if (and default-directory
		         base-file-name same-directory?)
		    (if (eq org-link-file-path-type 'adaptive)
		        (file-relative-name
		         result-file-name
                         (file-name-directory
			  base-file-name))
		      result-file-name)
		  result))
	      (if description (concat "[" description "]") "")))))

(defun org-babel-examplify-region (beg end &optional results-switches inline)
  "Comment out region BEG..END using the inline `==' or `: ' org example quote.
When INLINE is non-nil, use the inline verbatim markup.
When INLINE is nil and RESULTS-SWITCHES is non-nil, RESULTS-SWITCHES is
used as a string to be appended to #+begin_example line."
  (interactive "*r")
  (let ((maybe-cap
	 (lambda (str)
	   (if org-babel-uppercase-example-markers (upcase str) str))))
    (if inline
	(save-excursion
	  (goto-char beg)
	  (insert (format org-babel-inline-result-wrap
			  (delete-and-extract-region beg end))))
      (let ((size (count-lines beg end)))
	(save-excursion
	  (cond ((= size 0))	      ; do nothing for an empty result
		((< size org-babel-min-lines-for-block-output)
		 (goto-char beg)
		 (dotimes (_ size)
		   (forward-line 0) (insert ": ") (forward-line 1)))
		(t
		 (goto-char beg)
		 (insert (if results-switches
			     (format "%s%s\n"
				     (funcall maybe-cap "#+begin_example")
				     results-switches)
			   (funcall maybe-cap "#+begin_example\n")))
		 (let ((p (point)))
		   (if (markerp end) (goto-char end) (forward-char (- end beg)))
		   (org-escape-code-in-region p (point)))
		 (insert (funcall maybe-cap "#+end_example\n")))))))))

(defun org-babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (let ((element (org-element-at-point)))
    (unless (org-element-type-p element 'src-block)
      (error "Not in a source block"))
    (goto-char (org-babel-where-is-src-block-head element))
    (let* ((ind (org-current-text-indentation))
	   (body-start (line-beginning-position 2))
	   (body (org-element-normalize-string
		  (if (org-src-preserve-indentation-p element) new-body
		    (with-temp-buffer
		      (insert (org-remove-indentation new-body))
		      (indent-rigidly
		       (point-min)
		       (point-max)
		       (+ ind org-edit-src-content-indentation))
		      (buffer-string))))))
      (delete-region body-start
		     (org-with-wide-buffer
		      (goto-char (org-element-end element))
		      (skip-chars-backward " \t\n")
		      (line-beginning-position)))
      (goto-char body-start)
      (insert body))))

(defun org-babel-merge-params (&rest alists)
  "Combine all parameter association lists in ALISTS.
Later elements of ALISTS override the values of previous elements.
This takes into account some special considerations for certain
parameters when merging lists."
  (let* ((results-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'results org-babel-common-header-args-w-values))))
	 (exports-exclusive-groups
	  (mapcar (lambda (group) (mapcar #'symbol-name group))
		  (cdr (assq 'exports org-babel-common-header-args-w-values))))
	 (merge
	  (lambda (exclusive-groups &rest result-params)
	    ;; Maintain exclusivity of mutually exclusive parameters,
	    ;; as defined in EXCLUSIVE-GROUPS while merging lists in
	    ;; RESULT-PARAMS.
	    (let (output)
	      (dolist (new-params result-params (delete-dups output))
		(dolist (new-param new-params)
		  (dolist (exclusive-group exclusive-groups)
		    (when (member new-param exclusive-group)
		      (setq output (cl-remove-if
				    (lambda (o) (member o exclusive-group))
				    output))))
		  (push new-param output))))))
	 (variable-index 0)		;Handle positional arguments.
	 clearnames
	 params				;Final parameters list.
	 ;; Some keywords accept multiple values.  We need to treat
	 ;; them specially.
	 vars results exports)
    (dolist (alist alists)
      (dolist (pair alist)
	(pcase pair
	  (`(:var . ,value)
	   (let ((name (cond
                        ;; Default header arguments can accept lambda
                        ;; functions.  We uniquely identify the var
                        ;; according to the full string contents of
                        ;; the lambda function.
			((functionp value) value)
			((listp value) (car value))
			((string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*=" value)
			 (intern (match-string 1 value)))
			(t nil))))
	     (cond
	      (name
	       (setq vars
		     (append (if (not (assoc name vars)) vars
			       (push name clearnames)
			       (cl-remove-if (lambda (p) (equal name (car p)))
					     vars))
			     (list (cons name pair)))))
	      ((and vars (nth variable-index vars))
	       ;; If no name is given and we already have named
	       ;; variables then assign to named variables in order.
	       (let ((name (car (nth variable-index vars))))
		 ;; Clear out colnames and rownames for replace vars.
		 (push name clearnames)
		 (setf (cddr (nth variable-index vars))
		       (concat (symbol-name name) "=" value))
		 (cl-incf variable-index)))
	      (t (error "Variable \"%s\" must be assigned a default value"
			(cdr pair))))))
	  (`(:results . ,value)
	   (setq results (funcall merge
				  results-exclusive-groups
				  results
				  (split-string
				   (cond ((stringp value) value)
                                         ((functionp value) (funcall value))
                                         ;; FIXME: Arbitrary code evaluation.
                                         (t (eval value t)))))))
	  (`(:exports . ,value)
	   (setq exports (funcall merge
				  exports-exclusive-groups
				  exports
                                  (split-string
                                   (cond ((and value (functionp value)) (funcall value))
                                         (value value)
                                         (t ""))))))
          ((or '(:dir . attach) '(:dir . "'attach"))
           (unless (org-attach-dir nil t)
             (error "No attachment directory for element (add :ID: or :DIR: property)"))
           (setq params (append
                         `((:dir . ,(org-attach-dir nil t))
                           (:mkdirp . "yes"))
                         (assq-delete-all :dir (assq-delete-all :mkdir params)))))
	  ;; Regular keywords: any value overwrites the previous one.
	  (_ (setq params (cons pair (assq-delete-all (car pair) params)))))))
    ;; Handle `:var' and clear out colnames and rownames for replaced
    ;; variables.
    (setq params (nconc (mapcar (lambda (v) (cons :var (cddr v))) vars)
			params))
    (dolist (name clearnames)
      (dolist (param '(:colname-names :rowname-names))
	(when (assq param params)
	  (setf (cdr (assq param params))
		(cl-remove-if (lambda (pair) (equal name (car pair)))
			      (cdr (assq param params))))
	  (setq params
		(cl-remove-if (lambda (pair) (and (equal (car pair) param)
						  (null (cdr pair))))
			      params)))))
    ;; Handle other special keywords, which accept multiple values.
    (setq params (nconc (list (cons :results (mapconcat #'identity results " "))
			      (cons :exports (mapconcat #'identity exports " ")))
			params))
    ;; Return merged params.
    (org-babel-eval-headers params)))

(defun org-babel-noweb-p (params context)
  "Check if PARAMS require expansion in CONTEXT.
CONTEXT may be one of :tangle, :export or :eval."
  (let ((allowed-values (cl-case context
			  (:tangle '("yes" "tangle" "no-export" "strip-export" "strip-tangle"))
			  (:eval   '("yes" "no-export" "strip-export" "eval" "strip-tangle"))
			  (:export '("yes" "strip-tangle")))))
    (cl-some (lambda (v) (member v allowed-values))
	     (split-string (or (cdr (assq :noweb params)) "")))))

(defvar org-babel-expand-noweb-references--cache nil
  "Noweb reference cache used during expansion.")
(defvar org-babel-expand-noweb-references--cache-buffer nil
  "Cons (BUFFER . MODIFIED-TICK) for cached noweb references.
See `org-babel-expand-noweb-references--cache'.")
(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

When optional argument INFO is non-nil, use the block defined by INFO
instead.

The block is assumed to be located in PARENT-BUFFER or current buffer
\(when PARENT-BUFFER is nil).

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
	 (info (or info (org-babel-get-src-block-info 'no-eval)))
         (lang (nth 0 info))
         (body (nth 1 info))
	 (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
         (noweb-prefix (let ((v (assq :noweb-prefix (nth 2 info))))
                         (or (not v)
                             (and (org-not-nil (cdr v))
                                  (not (equal (cdr v) "no"))))))
	 (noweb-re (format "\\(.*?\\)\\(%s\\)"
			   (with-current-buffer parent-buffer
			     (org-babel-noweb-wrap)))))
    (unless (equal (cons parent-buffer
                         (with-current-buffer parent-buffer
                           (buffer-chars-modified-tick)))
                   org-babel-expand-noweb-references--cache-buffer)
      (setq org-babel-expand-noweb-references--cache nil
            org-babel-expand-noweb-references--cache-buffer
            (cons parent-buffer
                  (with-current-buffer parent-buffer
                    (buffer-chars-modified-tick)))))
    (cl-macrolet ((c-wrap
	            (s)
	            ;; Comment string S, according to LANG mode.  Return new
	            ;; string.
	            `(unless org-babel-tangle-uncomment-comments
	               (with-temp-buffer
		         (funcall (org-src-get-lang-mode lang))
		         (comment-region (point)
				         (progn (insert ,s) (point)))
		         (org-trim (buffer-string)))))
	          (expand-body
	            (i)
	            ;; Expand body of code represented by block info I.
	            `(let ((b (if (org-babel-noweb-p (nth 2 ,i) :eval)
			          (org-babel-expand-noweb-references ,i)
		                (nth 1 ,i))))
	               (if (not comment) b
		         (let ((cs (org-babel-tangle-comment-links ,i)))
		           (concat (c-wrap (car cs)) "\n"
			           b "\n"
			           (c-wrap (cadr cs)) "\n")))))
	          (expand-references
	            (ref)
	            `(pcase (gethash ,ref org-babel-expand-noweb-references--cache)
	               (`(,last . ,previous)
	                ;; Ignore separator for last block.
	                (let ((strings (list (expand-body last))))
		          (dolist (i previous)
		            (let ((parameters (nth 2 i)))
		              ;; Since we're operating in reverse order, first
		              ;; push separator, then body.
		              (push (or (cdr (assq :noweb-sep parameters)) "\n")
			            strings)
		              (push (expand-body i) strings)))
		          (mapconcat #'identity strings "")))
	               ;; Raise an error about missing reference, or return the
	               ;; empty string.
	               ((guard (or org-babel-noweb-error-all-langs
			           (member lang org-babel-noweb-error-langs)))
	                (error "Cannot resolve %s (see `org-babel-noweb-error-langs')"
		               (org-babel-noweb-wrap ,ref)))
	               (_ ""))))
      (replace-regexp-in-string
       noweb-re
       (lambda (m)
         (with-current-buffer parent-buffer
	   (save-match-data
	     (let* ((prefix (match-string 1 m))
		    (id (match-string 3 m))
		    (evaluate (string-match-p "(.*)" id))
		    (expansion
		     (cond
		      (evaluate
                       (prog1
		           (let ((raw (org-babel-ref-resolve id)))
		             (if (stringp raw) raw (format "%S" raw)))
                         ;; Evaluation can potentially modify the buffer
		         ;; and invalidate the cache: reset it.
                         (unless (equal org-babel-expand-noweb-references--cache-buffer
                                        (cons parent-buffer
                                              (buffer-chars-modified-tick)))
		           (setq org-babel-expand-noweb-references--cache nil
                                 org-babel-expand-noweb-references--cache-buffer
                                 (cons parent-buffer
                                       (with-current-buffer parent-buffer
                                         (buffer-chars-modified-tick)))))))
                      ;; Already cached.
                      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash id org-babel-expand-noweb-references--cache))
                       (expand-references id))
		      ;; Return the contents of headlines literally.
		      ((org-babel-ref-goto-headline-id id)
		       (org-babel-ref-headline-body))
		      ;; Look for a source block named SOURCE-NAME.  If
		      ;; found, assume it is unique; do not look after
		      ;; `:noweb-ref' header argument.
		      ((org-with-point-at 1
		         (let ((r (org-babel-named-src-block-regexp-for-name id)))
			   (and (re-search-forward r nil t)
			        (not (org-in-commented-heading-p))
                                (let ((info (org-babel-get-src-block-info t)))
                                  (unless (hash-table-p org-babel-expand-noweb-references--cache)
                                    (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal)))
                                  (push info (gethash id  org-babel-expand-noweb-references--cache))
			          (expand-body info))))))
		      ;; Retrieve from the Library of Babel.
		      ((nth 2 (assoc-string id org-babel-library-of-babel)))
		      ;; All Noweb references were cached in a previous
		      ;; run.  Yet, ID is not in cache (see the above
		      ;; condition).  Process missing reference in
		      ;; `expand-references'.
		      ((and (hash-table-p org-babel-expand-noweb-references--cache)
                            (gethash 'buffer-processed org-babel-expand-noweb-references--cache))
		       (expand-references id))
		      ;; Though luck.  We go into the long process of
		      ;; checking each source block and expand those
		      ;; with a matching Noweb reference.  Since we're
		      ;; going to visit all source blocks in the
		      ;; document, cache information about them as well.
		      (t
		       (setq org-babel-expand-noweb-references--cache (make-hash-table :test #'equal))
		       (org-with-wide-buffer
		        (org-babel-map-src-blocks nil
			  (if (org-in-commented-heading-p)
			      (org-forward-heading-same-level nil t)
			    (let* ((info (org-babel-get-src-block-info t))
				   (ref (cdr (assq :noweb-ref (nth 2 info)))))
			      (push info (gethash ref org-babel-expand-noweb-references--cache))))))
                       (puthash 'buffer-processed t org-babel-expand-noweb-references--cache)
		       (expand-references id)))))
	       ;; Interpose PREFIX between every line.
               (if noweb-prefix
		   (mapconcat #'identity
			      (split-string expansion "[\n\r]")
			      (concat "\n" prefix))
                 expansion)))))
       body t t 2))))

(defun org-babel--script-escape-inner (str)
  (let (in-single in-double backslash out)
    (mapc
     (lambda (ch)
       (setq
	out
	(if backslash
	    (progn
	      (setq backslash nil)
	      (cond
	       ((and in-single (eq ch ?'))
		;; Escaped single quote inside single quoted string:
		;; emit just a single quote, since we've changed the
		;; outer quotes to double.
		(cons ch out))
	       ((eq ch ?\")
		;; Escaped double quote
		(if in-single
		    ;; This should be interpreted as backslash+quote,
		    ;; not an escape.  Emit a three backslashes
		    ;; followed by a quote (because one layer of
		    ;; quoting will be stripped by `org-babel-read').
		    (append (list ch ?\\ ?\\ ?\\) out)
		  ;; Otherwise we are in a double-quoted string.  Emit
		  ;; a single escaped quote
		  (append (list ch ?\\) out)))
	       ((eq ch ?\\)
		;; Escaped backslash: emit a single escaped backslash
		(append (list ?\\ ?\\) out))
	       ;; Other: emit a quoted backslash followed by whatever
	       ;; the character was (because one layer of quoting will
	       ;; be stripped by `org-babel-read').
	       (t (append (list ch ?\\ ?\\) out))))
	  (cl-case ch
	    (?\[ (if (or in-double in-single)
		     (cons ?\[ out)
		   (cons ?\( out)))
	    (?\] (if (or in-double in-single)
		     (cons ?\] out)
		   (cons ?\) out)))
	    (?\{ (if (or in-double in-single)
		     (cons ?\{ out)
		   (cons ?\( out)))
	    (?\} (if (or in-double in-single)
		     (cons ?\} out)
		   (cons ?\) out)))
	    (?, (if (or in-double in-single)
		    (cons ?, out) (cons ?\s out)))
	    (?\' (if in-double
		     (cons ?\' out)
		   (setq in-single (not in-single)) (cons ?\" out)))
	    (?\" (if in-single
		     (append (list ?\" ?\\) out)
		   (setq in-double (not in-double)) (cons ?\" out)))
	    (?\\ (unless (or in-single in-double)
		   (error "Can't handle backslash outside string in `org-babel-script-escape'"))
		 (setq backslash t)
		 out)
	    (t  (cons ch out))))))
     (string-to-list str))
    (when (or in-single in-double)
      (error "Unterminated string in `org-babel-script-escape'"))
    (apply #'string (reverse out))))

(defun org-babel-script-escape (str &optional force)
  "Safely convert tables into elisp lists."
  (unless (stringp str)
    (error "`org-babel-script-escape' expects a string"))
  (let ((escaped
	 (cond
	  ((and (>= (length str) 2)
		(or (and (string-equal "[" (substring str 0 1))
			 (string-equal "]" (substring str -1)))
		    (and (string-equal "{" (substring str 0 1))
			 (string-equal "}" (substring str -1)))
		    (and (string-equal "(" (substring str 0 1))
			 (string-equal ")" (substring str -1)))))

	   (concat "'" (org-babel--script-escape-inner str)))
	  ((or force
	       (and (> (length str) 2)
		    (or (and (string-equal "'" (substring str 0 1))
			     (string-equal "'" (substring str -1)))
			;; We need to pass double-quoted strings
			;; through the backslash-twiddling bits, even
			;; though we don't need to change their
			;; delimiters.
			(and (string-equal "\"" (substring str 0 1))
			     (string-equal "\"" (substring str -1))))))
	   (org-babel--script-escape-inner str))
	  (t str))))
    (condition-case nil (org-babel-read escaped) (error escaped))))

(defun org-babel-read (cell &optional inhibit-lisp-eval)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if CELL looks like Lisp (meaning it starts with a
\"(\", \"\\='\", \"\\=`\" or a \"[\") then read and evaluate it as
lisp, otherwise return it unmodified as a string.  Optional
argument INHIBIT-LISP-EVAL inhibits lisp evaluation for
situations in which is it not appropriate."
  (cond ((not (org-string-nw-p cell)) cell)
	((org-babel--string-to-number cell))
	((and (not inhibit-lisp-eval)
	      (or (memq (string-to-char cell) '(?\( ?' ?` ?\[))
		  (string= cell "*this*")))
         ;; FIXME: Arbitrary code evaluation.
	 (eval (read cell) t))
	((let (read-val)
           (and (string-match-p
                 (rx bos (0+ (any space ?\n))
                     ?\" (0+ anychar) ?\"
                     (0+ (any space ?\n)) eos)
                 cell)
                ;; CELL is a single string
                (with-temp-buffer
                  (insert cell)
                  (goto-char 1)
                  (when (setq read-val
                              (ignore-errors
                                (read (current-buffer))))
                    (skip-chars-forward "[:space:]")
                    (eobp)))
                read-val)))
	(t (org-no-properties cell))))

(defun org-babel--string-to-number (string)
  "If STRING represents a number return its value.
Otherwise return nil."
  (unless (or (string-match-p "\\s-" (org-trim string))
	      (not (string-match-p "^[0-9e.+ -]+$" string)))
    (let ((interned-string (ignore-errors (read string))))
      (when (numberp interned-string)
	interned-string))))

(defun org-babel-import-elisp-from-file (file-name &optional separator)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar.
SEPARATOR is passed to `org-table-convert-region', which see."
  (let ((result
	 (with-temp-buffer
	   (condition-case err
	       (progn
		 (insert-file-contents file-name)
		 (delete-file file-name)
		 (let ((pmax (point-max)))
		   ;; If the file was empty, don't bother trying to
		   ;; convert the table.
		   (when (> pmax 1)
		     (org-table-convert-region
                      (point-min) pmax
                      (or separator 'babel-auto))
		     (delq nil
			   (mapcar (lambda (row)
				     (and (not (eq row 'hline))
					  (mapcar #'org-babel-string-read row)))
				   (org-table-to-lisp))))))
	     (error
	      (display-warning 'org-babel
			       (format "Error reading results: %S" err)
			       :error)
	      nil)))))
    (pcase result
      (`((,scalar)) scalar)
      (`((,_ ,_ . ,_)) result)
      (`(,scalar) scalar)
      (_ result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around CELL string.
When CELL is not a string, return CELL."
  (org-babel-read (or (and (stringp cell)
                           (string-match "^[[:space:]]*\"\\(.+\\)\"[[:space:]]*$" cell)
                           (match-string 1 cell))
                      cell) t))

(defun org-babel-chomp (string &optional regexp)
  "Strip a trailing space or carriage return from STRING.
The default regexp used is \"[ \\f\\t\\n\\r\\v]\" but another one
can be specified as the REGEXP argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0)
                (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))

(defun org-babel-process-file-name (name &optional no-quote-p)
  "Prepare NAME to be used in an external process.
If NAME specifies a remote location, the remote portion of the
name is removed, since in that case the process will be executing
remotely.  The file name is then processed by `expand-file-name'.
Unless second argument NO-QUOTE-P is non-nil, the file name is
additionally processed by `shell-quote-argument'."
  (let ((f (org-babel-local-file-name (expand-file-name name))))
    (if no-quote-p f (shell-quote-argument f))))

(defvar org-babel-temporary-directory
  (unless noninteractive
    (make-temp-file "babel-" t))
  "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown.")

(defvar org-babel-temporary-stable-directory
  (unless noninteractive
    (let (dir)
      (while (or (not dir) (file-exists-p dir))
        (setq dir (expand-file-name
                   (format "babel-stable-%d" (random 1000))
                   temporary-file-directory)))
      (make-directory dir)
      dir))
  "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown.")

(defcustom org-babel-remote-temporary-directory "/tmp/"
  "Directory to hold temporary files on remote hosts."
  :group 'org-babel
  :type 'string)

(defmacro org-babel-result-cond (result-params scalar-form &rest table-forms)
  "Call the code to parse raw string results according to RESULT-PARAMS.
Do nothing with :results discard.
Execute SCALAR-FORM when result should be treated as a string.
Execute TABLE-FORMS when result should be considered sexp and parsed."
  (declare (indent 1) (debug t))
  (org-with-gensyms (params)
    `(let ((,params ,result-params))
       (unless (member "discard" ,params)
         (if (or (member "scalar" ,params)
	         (member "verbatim" ,params)
	         (member "html" ,params)
	         (member "code" ,params)
	         (member "pp" ,params)
	         (member "file" ,params)
	         (and (or (member "output" ,params)
			  (member "raw"    ,params)
			  (member "org"    ,params)
			  (member "drawer" ,params))
		      (not (member "table" ,params))))
	     ,scalar-form
	   ,@table-forms)))))

(defmacro org-babel-temp-directory ()
  "Return temporary directory suitable for `default-directory'."
  `(if (file-remote-p default-directory)
       (concat (file-remote-p default-directory)
	       org-babel-remote-temporary-directory)
     (or (and org-babel-temporary-directory
	      (file-exists-p org-babel-temporary-directory)
	      org-babel-temporary-directory)
	 temporary-file-directory)))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of function `temporary-file-directory' temporarily set to the
value of `org-babel-temporary-directory'."
  (make-temp-file
   (concat (file-name-as-directory (org-babel-temp-directory)) prefix)
   nil
   suffix))

(defmacro org-babel-temp-stable-directory ()
  "Return temporary stable directory."
  `(let ((org-babel-temporary-directory org-babel-temporary-stable-directory))
     (org-babel-temp-directory)))

(defun org-babel-temp-stable-file (data prefix &optional suffix)
  "Create a temporary file in the `org-babel-remove-temporary-stable-directory'.
The file name is stable with respect to DATA.  The file name is
constructed like the following: <PREFIX><DATAhash><SUFFIX>."
  (let ((path
         (format
          "%s%s%s%s"
          (file-name-as-directory (org-babel-temp-stable-directory))
          prefix
          (org-sxhash-safe data)
          (or suffix ""))))
    ;; Create file.
    (with-temp-file path)
    ;; Return it.
    path))

(defun org-babel-remove-temporary-directory ()
  "Remove `org-babel-temporary-directory' on Emacs shutdown."
  (when (and org-babel-temporary-directory
	     (file-exists-p org-babel-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		(directory-files org-babel-temporary-directory 'full
				 directory-files-no-dot-files-regexp))
	  (delete-directory org-babel-temporary-directory))
      (error
       (message "Failed to remove temporary Org-babel directory %s"
		(or org-babel-temporary-directory
		    "[directory not defined]"))))))

(defun org-babel-remove-temporary-stable-directory ()
  "Remove `org-babel-temporary-stable-directory' and on Emacs shutdown."
  (when (and org-babel-temporary-stable-directory
	     (file-exists-p org-babel-temporary-stable-directory))
    (let ((org-babel-temporary-directory
           org-babel-temporary-stable-directory))
      (org-babel-remove-temporary-directory))))

(add-hook 'kill-emacs-hook #'org-babel-remove-temporary-directory)
(add-hook 'kill-emacs-hook #'org-babel-remove-temporary-stable-directory)

(defun org-babel-one-header-arg-safe-p (pair safe-list)
  "Determine if the PAIR is a safe babel header arg according to SAFE-LIST.

For the format of SAFE-LIST, see `org-babel-safe-header-args'."
  (and (consp pair)
       (keywordp (car pair))
       (stringp (cdr pair))
       (or
	(memq (car pair) safe-list)
	(let ((entry (assq (car pair) safe-list)))
	  (and entry
	       (consp entry)
	       (cond ((functionp (cdr entry))
		      (funcall (cdr entry) (cdr pair)))
		     ((listp (cdr entry))
		      (member (cdr pair) (cdr entry)))
		     (t nil)))))))

(defun org-babel-generate-file-param (src-name params)
  "Calculate the filename for source block results.

The directory is calculated from the :output-dir property of the
source block; if not specified, use the current directory.

If the source block has a #+NAME and the :file parameter does not
contain any period characters, then the :file parameter is
treated as an extension, and the output file name is the
concatenation of the directory (as calculated above), the block
name, a period, and the parameter value as a file extension.
Otherwise, the :file parameter is treated as a full file name,
and the output file name is the directory (as calculated above)
plus the parameter value."
  (let* ((file-cons (assq :file params))
	 (file-ext-cons (assq :file-ext params))
	 (file-ext (cdr-safe file-ext-cons))
	 (dir (cdr-safe (assq :output-dir params)))
	 fname)
    ;; create the output-dir if it does not exist
    (when dir
      (make-directory dir t))
    (if file-cons
	;; :file given; add :output-dir if given
	(when dir
	  (setcdr file-cons (concat (file-name-as-directory dir) (cdr file-cons))))
      ;; :file not given; compute from name and :file-ext if possible
      (when (and src-name file-ext)
	(if dir
	    (setq fname (concat (file-name-as-directory (or dir ""))
				src-name "." file-ext))
	  (setq fname (concat src-name "." file-ext)))
	(setq params (cons (cons :file fname) params))))
    params))

(defun org-babel-graphical-output-file (params)
  "File where a babel block should send graphical output, per PARAMS.
Return nil if no graphical output is expected.  Raise an error if
the output file is ill-defined."
  (let ((file (cdr (assq :file params))))
    (cond (file (and (member "graphics" (cdr (assq :result-params params)))
		     file))
	  ((assq :file-ext params)
	   (user-error ":file-ext given but no :file generated; did you forget \
to name a block?"))
	  (t (user-error "No :file header argument given; cannot create \
graphical result")))))

(defun org-babel-make-language-alias (new old)
  "Make source blocks of type NEW aliases for those of type OLD.

NEW and OLD should be strings.  This function should be called
after the babel API for OLD-type source blocks is fully defined.

Callers of this function will probably want to add an entry to
`org-src-lang-modes' as well."
  (dolist (fn '("execute" "expand-body" "prep-session"
		"variable-assignments" "load-session"
		"edit-prep"))
    (let ((sym (intern-soft (concat "org-babel-" fn ":" old))))
      (when (and sym (fboundp sym))
	(defalias (intern (concat "org-babel-" fn ":" new)) sym))))
  ;; Technically we don't need a `dolist' for just one variable, but
  ;; we keep it for symmetry/ease of future expansion.
  (dolist (var '("default-header-args"))
    (let ((sym (intern-soft (concat "org-babel-" var ":" old))))
      (when (and sym (boundp sym))
	(defvaralias (intern (concat "org-babel-" var ":" new)) sym)))))

(provide 'ob-core)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ob-core.el ends here
