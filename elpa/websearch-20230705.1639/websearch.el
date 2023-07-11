;;; websearch.el --- Query search engines -*- lexical-binding: t -*-


;; This file is part of websearch - query search engines from Emacs.
;; Copyright (c) 2022-2023, Maciej Barć <xgqt@riseup.net>
;; Licensed under the GNU GPL v2 License
;; SPDX-License-Identifier: GPL-2.0-or-later

;; websearch is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; websearch is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with websearch.  If not, see <https://www.gnu.org/licenses/>.


;; Author: Maciej Barć <xgqt@riseup.net>
;; Homepage: https://gitlab.com/xgqt/emacs-websearch/
;; Version: 2.1.1
;; Keywords: convenience hypermedia
;; Package-Requires: ((emacs "24.4"))
;; SPDX-License-Identifier: GPL-2.0-or-later



;;; Commentary:


;; Query search engines from Emacs.

;; The websearch package allows You to query predefined search engines
;; (‘websearch-custom-engines’) with interactive selection.
;; The query terms can either be extracted form selection, kill-ring
;; or typed on demand.

;; The `websearch' function is a interactive entry-point to select both
;; the terms extraction method and search engine provider.

;; To turn on the global mode enabling a custom key map,
;; activate `websearch-mode'.

;; ‘websearch’ is inspired by ‘engine-mode’
;; (https://github.com/hrs/engine-mode), but the differences are big enough
;; for it to be it's own package.

;; The full set of commands you can try is:
;; * websearch-mode
;; * websearch
;; * websearch-browse-with
;; * websearch-kill-ring
;; * websearch-list-engines
;; * websearch-point
;; * websearch-region
;; * websearch-term

;; For more information and screenshots, see:
;; https://gitlab.com/xgqt/emacs-websearch/



;;; Code:


(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)

(require 'websearch-custom)


(defgroup websearch nil
  "Query search engines from Emacs."
  :group 'convenience
  :group 'external
  :group 'hypermedia
  :group 'web)


(defconst websearch-version "2.1.1"
  "Search-Engine package version.")

(defconst websearch-methods
  '(("point"     . websearch-point)
    ("region"    . websearch-region)
    ("kill-ring" . websearch-kill-ring)
    ("term"      . websearch-term))
  "Methods of `websearch'.

Each element is an association pair composed of a method name and a function
that is defined in Search-Engine package.")


(defun websearch--methods-names ()
  "Return the names of ‘websearch-methods’."
  (mapcar #'car websearch-methods))

(defun websearch--method-value (method-name)
  "Return value of METHOD-NAME from ‘websearch-methods’."
  (cdr (assoc method-name websearch-methods)))

(defun websearch--select-engines (&optional selected-engine-string)
  "Return the query URL.

URL is extracted from associated with SELECTED-ENGINE-STRING
or from completing read."
  (let* ((engine-names
          (websearch--engine-names))
         (selected-engine-string
          (if selected-engine-string
              selected-engine-string
            (completing-read "Search engine: "
                             (append websearch-custom-groups
                                     (websearch--all-tags-encoded)
                                     engine-names)
                             nil
                             t
                             websearch-custom-default-engine)))
         (selected-engines
          (let ((tag
                 (websearch--decode-tag selected-engine-string)))
            (cond
             (tag
              (websearch--tag-matches tag))
             (t
              (mapcar (lambda (engine-name)
                        (assoc (string-trim engine-name)
                               websearch-custom-engines))
                      (split-string selected-engine-string ",")))))))
    selected-engines))

(defun websearch--form-query (query-url separator search-term)
  "Form a full search URL query.

Returns URL formed from formatted QUERY-URL, SEPARATOR and SEARCH-TERM."
  (let ((query-search-term
         (cond
          ((or (equal separator ?\s) (equal separator " "))
           search-term)
          (t
           (let ((sep
                  (if (stringp separator) separator (string separator))))
             (replace-regexp-in-string " " sep search-term))))))
    (concat "https://" query-url (url-hexify-string query-search-term))))

(defun websearch--browse-url (search-term &optional engines)
  "Browse the full query URL.

SEARCH-TERM is given to a search ENGINES given directly to function
or selected interactively by the user."
  (let* ((engines
          (if engines (websearch--select-engines engines)
            (websearch--select-engines)))
         (query-urls
          (mapcar (lambda (engine) (nth 2 engine)) engines))
         (separators
          (mapcar (lambda (engine) (nth 1 engine)) engines)))
    (cl-mapc (lambda (query-url separator)
               ;; TODO: Async? -- to open both at the same time.
               (funcall websearch-custom-browse-url-function
                        (websearch--form-query query-url
                                               separator
                                               search-term)))
             query-urls
             separators)))


;;;###autoload
(defun websearch-browse-with (browse-url-function)
  "Set the function used to browse full query URLs to BROWSE-URL-FUNCTION."
  (interactive
   (list (intern (completing-read
                  "Browse with function: "
                  websearch-custom-browse-url-function-candidates))))
  (message "Selected function: %s" browse-url-function)
  (setq-default websearch-custom-browse-url-function browse-url-function))

;;;###autoload
(defun websearch-point ()
  "Query search engines based on `thing-at-point'."
  (interactive)
  (websearch--browse-url (thing-at-point 'symbol 'no-properties)))

;;;###autoload
(defun websearch-region (start end)
  "Query search engines based on selected buffer region.

START and END come from the selected region, they form the search term."
  (interactive "r")
  (let ((search-term (buffer-substring start end)))
    (websearch--browse-url search-term)))

;;;###autoload
(defun websearch-kill-ring ()
  "Query search engines based on ‘kill-ring’."
  (interactive)
  (let* ((kill-ring-contents
          (mapcar #'substring-no-properties kill-ring))
         (search-term
          (completing-read "Term from kill-ring: " kill-ring-contents)))
    (websearch--browse-url search-term)))

;;;###autoload
(defun websearch-term (search-term)
  "Query search engines based on SEARCH-TERM input from prompt."
  (interactive "sSearch term: ")
  (websearch--browse-url search-term))

;;;###autoload
(defun websearch (method-name)
  "Query search engines with a METHOD-NAME.

The list of possible selections is defined by ‘websearch-methods’."
  (interactive
   (let* ((method-names
           (websearch--methods-names))
          (method-name
           (completing-read "Search method: " method-names nil t)))
     (list method-name)))
  (call-interactively (websearch--method-value method-name)))


;;;###autoload
(cl-defmacro websearch-define (engine-name &key
                                           docstring
                                           keybinding
                                           (function t)
                                           query-separator
                                           query-url
                                           (tags '("generic")))
  "Define a dwim function to search the web using `websearch'.
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
i.e. \":tags (\"text\" \"generic\")\""
  (declare (indent 2)
           (doc-string 3))
  (cl-assert (stringp engine-name))
  (let* ((engine-name-sym (intern engine-name))
         (tagp (string-match-p "#" engine-name))
         (query-separator (when query-separator query-separator))
         (tags (when tags (mapcar 'intern tags)))
         (engine-list (list engine-name query-separator query-url tags))
         (engine-group  (string-match-p "," engine-name))
         (func-name (concat "websearch-" (replace-regexp-in-string ", \\|," "-" engine-name)))
         (func (intern func-name)))
    (when (and query-separator query-url tags (not (or tagp engine-group))) (add-to-list 'websearch-custom-engines engine-list))
    (if tagp
        (unless (member engine-name (websearch--all-tags-encoded))
          (user-error "\"%s\" not a tag from `websearch-custom-engines'" engine-name))
      (unless (assoc engine-name websearch-custom-engines)
        (user-error "\"%s\" not a member of `websearch-custom-engines'
Please check your spelling, or add QUERY-SEPARATOR, QUERY-URL and TAGS to add
%s to `websearch-custom-engines'" engine-name engine-name)))
    (when (and function keybinding)
      (funcall (lambda (func)
                 (define-key websearch-mode-map
                   (kbd (concat websearch-custom-keymap-prefix " " keybinding)) func))
               func))
    (when function
      `(progn
         (defun ,func (search-term &optional arg)
           ,(concat docstring (when docstring "\n")
                    (format "Search %s for SEARCH-TERM with `websearch'.
SEARCH-TERM is region if region is selected.
When called with prefix ARG use `kill-ring' for completions"
                            (capitalize engine-name)))
           (interactive
            (let* ((arg current-prefix-arg)
                   (region (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
                   (kill (mapcar #'substring-no-properties kill-ring))
                   (thing-at-point (when (thing-at-point 'symbol) (substring-no-properties (thing-at-point 'symbol))))
                   (completions (if arg kill
                                  (remove nil(delete-dups
                                              (list thing-at-point kill))))))
              (if (use-region-p) (list region) (list (completing-read "Search term: " completions)))))
           (websearch--browse-url search-term ,engine-name))))))

;;;###autoload
(cl-defmacro websearch-define-group (group-name &key
                                                keybinding
                                                (function t)
                                                docstring)
  "Define a new function for searching a group of web engines using `websearch'.
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
to the specified key."
  (declare (indent 2)
           (docstring 3))
  (let* ((commap
          (string-match-p "," group-name))
         (func-name
          (concat "websearch-group-"
                  (replace-regexp-in-string ", \\|," "-" group-name)))
         (func
          (intern func-name))
         (group-defined-p
          (member group-name websearch-custom-groups))
         (group-engines
          (split-string group-name ", \\|,"))
         (group-engines-p
          (eq (length group-engines)
              (length (remove nil
                              (mapcar (lambda (engine)
                                        (assoc engine websearch-custom-engines))
                                      group-engines))))))
    (unless commap
      (user-error "Group not properly named, should be separated with a comma
e.g. \"google, duckduckgo, yandex\""))
    (unless group-defined-p
      (if group-engines-p
          (push group-name websearch-custom-groups)
        (let ((non-engines (remove nil (mapcar (lambda (engine)
                                                 (when (not (assoc engine websearch-custom-engines))
                                                   engine))
                                               group-engines))))
          (user-error "\"%s\" not a member of `websearch-custom-engines'
Pleas add %s to `websearch-custom-engines' and try again" non-engines
non-engines))))
    (when (and function keybinding)
      (funcall (lambda (func)
                 (define-key websearch-mode-map
                   (kbd (concat websearch-custom-keymap-prefix " " keybinding)) func))
               func))
    (when function
      `(progn
         (defun ,func (search-term &optional arg)
           ,(concat docstring (when docstring "\n")
                    (format "Search %s for SEARCH-TERM with `websearch'.
SEARCH-TERM is region if region is selected.
When called with prefix ARG use `kill-ring' for completions"
                            (let* ((split-group
                                    (split-string group-name ", \\|,"))
                                   (last-engine
                                    (car (last split-group)))
                                   (upcased-group
                                    (mapconcat (lambda (x) (capitalize x)) split-group ", "))
                                   (formatted-group
                                    (concat (replace-regexp-in-string (concat ", " last-engine)
                                                                      (concat " & " (capitalize last-engine))
                                                                      upcased-group))))
                              formatted-group)))
           (interactive
            (let* ((arg current-prefix-arg)
                   (region (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))
                   (kill (mapcar #'substring-no-properties kill-ring))
                   (thing-at-point (when (thing-at-point 'symbol) (substring-no-properties (thing-at-point 'symbol))))
                   (completions (if arg kill
                                  (remove nil(delete-dups
                                              (list thing-at-point kill))))))
              (if (use-region-p) (list region) (list (completing-read "Search term: " completions)))))
           (websearch--browse-url search-term ,group-name))))))


(provide 'websearch)



;;; websearch.el ends here
