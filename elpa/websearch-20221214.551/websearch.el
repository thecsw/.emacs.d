;;; websearch.el --- Query search engines -*- lexical-binding: t -*-


;; This file is part of websearch - query search engines from Emacs.
;; Copyright (c) 2022, Maciej Barć <xgqt@riseup.net>
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
;; Version: 2.0.1
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



;;; Code:


(require 'cl-lib)
(require 'subr-x)

(require 'websearch-custom)


(defgroup websearch nil
  "Query search engines from Emacs."
  :group 'convenience
  :group 'external
  :group 'hypermedia
  :group 'web)


(defconst websearch-version "2.0.0"
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

(defun websearch--select-engines ()
  "Return the query URL.

URL is extracted from associated with search engine
selected from completing read."
  (let* ((engine-names
          (websearch--engine-names))
         (selected-engine-string
          (completing-read "Search engine: "
                           (append websearch-custom-groups
                                   (websearch--all-tags-encoded)
                                   engine-names)
                           nil
                           t
                           websearch-custom-default-engine))
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
          ((equal separator ?\s)
           search-term)
          (t
           (replace-regexp-in-string " " (string separator) search-term)))))
    (concat "https://" query-url (url-hexify-string query-search-term))))

(defun websearch--browse-url (search-term)
  "Browse the full query URL.

SEARCH-TERM is given to a search engine selected interactively by the user."
  (let* ((engines
          (websearch--select-engines))
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


(provide 'websearch)



;;; websearch.el ends here
