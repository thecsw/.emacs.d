;;; kql-mode.el --- Major mode for highlighting KQL -*- lexical-binding: t -*-

;; Author: Aimé Bertrand
;; Package-Version: 20250126.2159
;; Package-Revision: e7504518e0fe
;; Package-Requires: ((emacs "26.1"))
;; Created: 2023-10-13
;; Keywords: files languages azure entra kql faces syntax major-mode
;; Homepage: https://gitlab.com/aimebertrand/kql-mode
;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2023 Aimé Bertrand
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; kql-mode provides syntax highlighting for Kusto Query Language (KQL).
;; To use, add the following to your Emacs configuration:
;;
;; I. Installation
;;   A. Manual installation
;;     1. Download the `kql-mode.el' file and add it to your `custom-load-path'.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode))
;;
;;   B. From Melpa
;;     1. M-x package-install RET kql-mode RET.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode))
;;
;;   C. With `use-package'
;;     (use-package kql-mode
;;       :ensure t
;;       :config
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode)))
;;
;; II.  Activating the mode interactively
;;    M-x kql-mode RET


;;; Code:

(defconst kql-mode-keywords
  '("add" "and" "as" "asc" "between" "by" "case" "contains" "count" "desc"
    "distinct" "endtime" "extend" "false" "fork" "format_datetime" "from" "has"
    "in" "ingestion_time" "inner" "isempty" "isfinite" "isnan" "isnotempty"
    "isnotnull" "isnull" "join" "let" "like" "limit" "make_list" "make_set"
    "matches regex" "not" "now" "or" "order by" "parse" "print" "project"
    "project-away" "project-rename" "range" "reduce" "render" "sample" "search"
    "serialize" "set" "starttime" "summarize" "take" "to" "todatetime"
    "todecimal" "todouble" "toint" "tolong" "tostring" "true" "type" "union"
    "where")
  "List of KQL keywords for syntax highlighting.
This list is not exhaustive for now.")

(defconst kql-mode-keywords-regexp
  (concat "\\b" (regexp-opt kql-mode-keywords 'words) "\\b")
  "Regexp to match KQL keywords for font-lock highlighting.")

(defconst kql-functions
  '("abs" "ago" "arg_max" "arg_min" "array_concat" "array_length" "array_slice"
    "autocluster" "avg" "bag_keys" "bag_merge" "base64_decode_tostring"
    "base64_encode_tostring" "basket" "bin" "bin_at" "ceiling" "count"
    "countif" "datetime" "dayofmonth" "dayofweek" "dayofyear" "dcount"
    "dcountif" "diffpatterns" "endofday" "endofmonth" "endofweek" "endofyear"
    "exp" "extract" "extract_all" "floor" "format_datetime" "format_timespan"
    "geo_distance_2points" "geo_point_in_polygon" "geo_polygon_to_s2cells"
    "getmonth" "getyear" "hash" "indexof" "isempty" "isnotempty" "log" "log10"
    "log2" "make_datetime" "make_timespan" "max" "min" "new_guid" "now" "pack"
    "parse_command_line" "parse_csv" "parse_json" "parse_json" "parse_url"
    "percentile" "pow" "predict" "rand" "replace" "round" "series_decompose"
    "series_fill_const" "series_fill_linear" "series_outliers" "series_stats"
    "split" "sqrt" "startofday" "startofmonth" "startofweek" "startofyear"
    "stdev" "strcat" "strlen" "substring" "sum" "take_any" "tobool" "todatetime"
    "todouble" "toint" "tolong" "tolower" "tostring" "totimespan" "toupper"
    "trim" "trim_end" "trim_start" "variance" "zip")
  "List of KQL functions for syntax highlighting.
This list is not exhaustive for now.")

(defconst kql-functions-regexp
  (concat "\\b" (regexp-opt kql-functions 'words) "\\b")
  "Regexp to match KQL functions for font-lock highlighting.")

(defconst kql-array-regexp
  "\\[\\([0-9]+\\)\\]"
  "Regexp to match KQL arrays for font-lock highlighting.")

(defconst kql-comment-regexp
  "//.*$"
  "Regexp to match KQL comments for font-lock highlighting.")

(defconst kql-functions-call-regexp
  (concat "\\b" (regexp-opt kql-functions 'words) "\\s-*(")
  "Regexp to match KQL function calls for font-lock highlighting.")

(defconst kql-operators-regexp
  (concat "\\(?:\\+\\|-\\|\\*\\|/\\|%\\|==\\|!=\\|>\\|"
          "<\\|>=\\|<=\\|!\\|=~\\|!~\\|in\\|!in\\)")
  "Regexp to match KQL operators for font-lock highlighting.")

(defconst kql-property-regexp
  "\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
  "Regexp to match KQL properties for font-lock highlighting.")

(defconst kql-string-regexp
  "\"\\(?:[^\"\\]\\|\\\\.\\)*\""
  "Regexp to match KQL strings for font-lock highlighting.")

(defconst kql-variable-regexp
  "\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-?=[^=]"
  "Regexp to match KQL variables for font-lock highlighting.")

(defconst kql-mode-font-lock-keywords
  `((,kql-array-regexp . font-lock-type-face)
    (,kql-comment-regexp . font-lock-comment-face)
    (,kql-functions-call-regexp . font-lock-function-name-face)
    (,kql-mode-keywords-regexp . font-lock-keyword-face)
    (,kql-functions-regexp . font-lock-function-name-face)
    (,kql-operators-regexp . font-lock-warning-face)
    (,kql-property-regexp . font-lock-function-name-face)
    (,kql-string-regexp . font-lock-string-face)
    (,kql-variable-regexp 1 font-lock-variable-name-face))
  "List of font-lock rules for syntax highlighting in KQL mode.")

(defvar kql-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Brackets and parentheses
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    ;; Comments
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Variables
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    table)
  "Syntax table for KQL mode.
Defines syntax rules for:
- Brackets and parentheses
- Comments
- Operators
- Strings
- Variables")

;;;###autoload
(define-derived-mode kql-mode prog-mode "KQL"
  "Major mode for highlighting Kusto Query Language (KQL) files."
  :syntax-table kql-mode-syntax-table
  (setq font-lock-defaults '(kql-mode-font-lock-keywords)))

(provide 'kql-mode)

;;; kql-mode.el ends here
