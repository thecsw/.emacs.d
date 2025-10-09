;;; hare-mode.el --- Hare mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamín Buccianti
;; Copyright (C) 2020 Amin Bandali
;; Copyright (C) 2020 Theodor Thornhill
;; Copyright (C) 2022 Sebastian
;; Copyright (C) 2024-2025 Alexander Grafov
;;
;; Author: Benjamín Buccianti <benjamin@buccianti.dev>
;;         Amin Bandali <bandali@gnu.org>
;;         Theodor Thornhill <theo@thornhill.no>
;;         Sebastian <sebastian@sebsite.pw>
;;         Alexander Grafov <grafov@inet.name>
;;
;; Keywords: languages
;; URL: https://git.sr.ht/~grafov/hare-mode
;; Package-Version: 20251002.1915
;; Package-Revision: bd7e2e8b1437
;; Package-Requires: ((emacs "25.1"))

;; Hare mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Hare mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Hare mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing `hare' files in GNU Emacs.

;;; Code:

(require 'smie)

(defvar hare-mode-keywords
  '("abort" "align" "alloc" "append" "as" "assert" "break" "case" "const"
    "continue" "def" "defer" "delete" "done" "else" "export" "fn" "for"
    "free" "if" "insert" "is" "len" "let" "match" "never" "nomem"
    "offset" "opaque" "return" "static" "switch" "type" "use" "vaarg"
    "vaend" "vastart" "yield" "_")
  "Keywords used in `hare-mode'.")

(defvar hare-mode-types
  '("u8" "u16" "u32" "u64" "i8" "i16" "i32" "i64" "int" "uint"
    "uintptr" "f32" "f64" "bool" "char" "size" "str" "void" "struct" "union"
    "nullable" "null" "valist" "rune" "enum")
  "Types used in `hare-mode'.")

(defvar hare-mode-constants
  '("true" "false")
  "Constants used in `hare-mode'.")

(defvar hare-mode-builtins
  '("@init" "@symbol" "@packed" "@test" "@fini" "@offset" "@noreturn"
    "@threadlocal")
  "Built in identifiers used in `hare-mode'.")

(defconst hare-mode--regexp-declaration-line-beginning
  (concat "^" (regexp-opt hare-mode-keywords))
  "Regexp matching `hare-mode-keywords' on line beginning.")

(defconst hare-mode--regexp-declaration-end
  (regexp-opt '("};" ");"))
  "Regexp matching declaration endings.")

(defconst hare-mode-indent-offset 8
  "Indent hare code by this number of spaces.")

(defvar hare-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'hare-mode-indent-forward)
    (define-key map (kbd "<backtab>") 'hare-mode-indent-backward)
    map)
  "Keymap for `hare-mode'.")

(defvar hare-mode-font-lock-defaults
  `((("\"\\.\\*\\?" . font-lock-string-face)
     (,(regexp-opt hare-mode-keywords 'symbols) . font-lock-keyword-face)
     (,(regexp-opt hare-mode-constants 'symbols) . font-lock-constant-face)
     (,(regexp-opt hare-mode-builtins 'symbols) . font-lock-builtin-face)
     (,(regexp-opt hare-mode-types 'symbols) . font-lock-type-face))))

(defconst hare-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; strings and characters
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; comments
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n  ">" st)

    ;; @ is part of symbols in Hare
    (modify-syntax-entry ?@ "_" st)

    ;; return our modified syntax table
    st))

(defvar hare-mode-imenu-generic-expression
  `(;; Functions
    (nil
     ,(concat
       (rx line-start)
       (rx (opt "export" (1+ blank)))
       (rx (opt "@" (or "test" "init" "fini") (1+ blank)))
       (rx (opt "@symbol(" (regexp "\".*\"") ")" (1+ blank)))
       (rx "fn" (1+ blank))
       (rx (group (or letter "_") (0+ (or letter "_" digit)))) ;; identifier
       (rx (0+ (syntax whitespace)))
       ;; Optional parameter list
       (rx (opt (syntax open-parenthesis)
		(0+ (any letter ":" "*" "," "_" "[" "]" digit whitespace))
		(syntax close-parenthesis)))
       (rx (0+ (syntax whitespace)))
       (rx (opt (1+ letter)))  ;; Optional nullable
       (rx (0+ (syntax whitespace)))
       (rx (opt (1+ letter)))  ;; Optional const
       (rx (0+ (syntax whitespace)))
       (rx (opt (or "*" "&")) (or (1+ (any letter ":")))) ;; result type
       (rx (0+ (syntax whitespace)))
       "=")
     1)))

(defun hare-mode-beginning-of-defun (&optional arg)
  "Jump to the beginning of a \"defun\".

Search backwards for ARG amount of occurrences of identifiers
represented by `hare-mode--regexp-declaration-line-beginning'."
  (interactive "p")
  (unless arg (setq arg 1))
  (re-search-backward hare-mode--regexp-declaration-line-beginning nil t arg))

(defun hare-mode-end-of-defun (&optional arg)
  "Jump to the end of a \"defun\".

Search backwards for ARG amount of occurrences of identifiers
represented by `hare-mode--regexp-declaration-end'."
  (interactive "p")
  (unless arg (setq arg 1))
  (re-search-forward hare-mode--regexp-declaration-end nil t arg))

(defun hare-mode--do-indent (indent)
  "Helper to indent a line to the column at INDENT."
  (if (<= (current-column) (current-indentation))
      (ignore-errors (indent-line-to indent))
    (save-excursion (ignore-errors (indent-line-to indent)))))

(defun hare-mode-indent-forward (&optional arg)
  "Indent line to the next ARG tabstop."
  (interactive "p")
  (or arg (setq arg 1))
  (hare-mode--do-indent
   (indent-next-tab-stop (* arg (current-indentation)))))

(defun hare-mode-indent-backward (&optional arg)
  "Indent backwards to the nearest tabstop.
ARG tabstops from the `current-column'"
  (interactive "p")
  (or arg (setq arg 1))
  (hare-mode--do-indent
   (indent-next-tab-stop
    (save-excursion (back-to-indentation) (current-column)) t)))

(defun hare-mode--backward-token ()
  "Same as `smie-default-backward-token'.

Expand this further on to get better parsing.  Put things in here
before changing the `hare-mode--smie-grammar'."
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-backward "."))
	 (skip-syntax-backward "w_'"))
     (point))))

(defun hare-mode--forward-token ()
  "Same as `smie-default-forward-token'.

Expand this further on to get better parsing.  Put things in here
before changing the `hare-mode--smie-grammar'."
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-forward "."))
	 (skip-syntax-forward "w_'"))
     (point))))

(defconst hare-mode--smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (branches (branches "|" branches))
       (toplevel (toplevel "," toplevel)
		 (toplevel ";" toplevel)))
     '((assoc "|"))
     '((assoc ";") (assoc ",")))
    (smie-precs->prec2
     '((assoc "+" "-" "^")
       (assoc "/" "*" "%")
       (nonassoc "==" "!="))))))

(defun hare-mode-smie-rules (kind token)
  "Rules for the smie grammar.

Argument KIND is one of \":elem\" \":before\" \":after\"
\":list-intro\", and is used to designate how to indent.

Argument TOKEN is the token in question, either defined by SMIE
or a custom made one.  See info manual for the most thorough
documentation of this feature."
  (pcase (cons kind token)
    (`(:elem . basic) hare-mode-indent-offset)
    (`(:after . ",") (smie-rule-separator kind))
    (`(:before . ";") (smie-rule-parent))
    (`(:after . ";") (smie-rule-separator kind))
    (`(:before . ,(or "(" "[" "{")) (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "=") (if (smie-rule-hanging-p) hare-mode-indent-offset))))

;;;###autoload
(define-derived-mode hare-mode prog-mode "Hare"
  "Major mode for editing `hare' files."
  :syntax-table hare-mode-syntax-table

  (setq-local tab-width hare-mode-indent-offset)
  (setq-local indent-tabs-mode t)
  (when (boundp 'electric-indent-inhibit) (setq electric-indent-inhibit t))

  (smie-setup hare-mode--smie-grammar #'hare-mode-smie-rules
	      :forward-token #'hare-mode--forward-token
	      :backward-token #'hare-mode--backward-token)

  (setq-local beginning-of-defun-function #'hare-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'hare-mode-end-of-defun)

  (setq-local font-lock-defaults hare-mode-font-lock-defaults)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq imenu-generic-expression hare-mode-imenu-generic-expression)
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ha\\'" . hare-mode))

(provide 'hare-mode)
;;; hare-mode.el ends here
