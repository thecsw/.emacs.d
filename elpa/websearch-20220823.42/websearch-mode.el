;;; websearch-mode.el --- Minor mode for websearch -*- lexical-binding: t -*-


;; This file is part of emacs-websearch.

;; emacs-websearch is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; emacs-websearch is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-websearch.  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (c) 2022, Maciej Barć <xgqt@riseup.net>
;; Licensed under the GNU GPL v3 License



;;; Commentary:


;; Customization for ‘websearch’.



;;; Code:


(require 'websearch-custom)
(require 'websearch)


(defun websearch-mode--kbd (keys-string)
  "Return KEYS-STRING prefixed with ‘websearch-custom-keymap-prefix’."
  (kbd (format "%s %s " websearch-custom-keymap-prefix keys-string)))


;;;###autoload
(define-minor-mode websearch-mode
  "Search engine minor mode."
  :global t
  :keymap
  (list (cons (websearch-mode--kbd "b") #'websearch-browse-with)
        (cons (websearch-mode--kbd "p") #'websearch-point)
        (cons (websearch-mode--kbd "r") #'websearch-region)
        (cons (websearch-mode--kbd "k") #'websearch-kill-ring)
        (cons (websearch-mode--kbd "t") #'websearch-term)
        (cons (websearch-mode--kbd "s") #'websearch))
  :group 'websearch)

(define-derived-mode websearch-engines-mode tabulated-list-mode
  "WebSearch Engines"
  "Major mode for listing ‘websearch-custom-engines’."
  (setq tabulated-list-format
        [("Name"      20 t)
         ("Separator" 10 t)
         ("Query URL" 60 t)
         ("Tags"      30 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (setq tabulated-list-entries
        (let ((index 0))
          (mapcar (lambda (entry)
                    (setq index (+ index 1))
                    (list index
                          (vector (nth 0 entry)
                                  (format "\"%c\"" (nth 1 entry))
                                  (concat "https://" (nth 2 entry))
                                  (mapconcat #'symbol-name (nth 3 entry) " "))))
                  websearch-custom-engines)))
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;;###autoload
(defun websearch-list-engines ()
  "Display a list of supported search engines."
  (interactive)
  (let ((buffer (get-buffer-create "*WebSearch Engines*")))
    (with-current-buffer buffer
      (websearch-engines-mode))
    (switch-to-buffer buffer)))


(provide 'websearch-mode)



;;; websearch-mode.el ends here
