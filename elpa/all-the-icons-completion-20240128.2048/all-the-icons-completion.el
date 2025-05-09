;;; all-the-icons-completion.el --- Add icons to completion candidates -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Itai Y. Efrat
;;
;; Author: Itai Y. Efrat <https://github.com/iyefrat>
;; Maintainer: Itai Y. Efrat <itai3397@gmail.com>
;; Created: June 06, 2021
;; Modified: June 06, 2021
;; Package-Version: 20240128.2048
;; Package-Revision: 4c8bcad8033f
;; Keywords: convenient, lisp
;; Homepage: https://github.com/iyefrat/all-the-icons-completion
;; Package-Requires: ((emacs "26.1") (all-the-icons "5.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licence:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Add icons to completion candidates.
;;
;;; Code:

(require 'all-the-icons)

(defgroup all-the-icons-completion nil
  "Add icons to completion candidates."
  :version "26.1"
  :group 'appearance
  :group 'convenience
  :prefix "all-the-icons-completion")

(defface all-the-icons-completion-dir-face
  '((t nil))
  "Face for the directory icon."
  :group 'all-the-icons-faces)

(cl-defgeneric all-the-icons-completion-get-icon (_cand _cat)
  "Return the icon for the candidate CAND of completion category CAT."
  "")

(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql file)))
  "Return the icon for the candidate CAND of completion category file."
  (cond ((string-match-p "\\/$" cand)
         (concat
          (all-the-icons-icon-for-dir cand :face 'all-the-icons-completion-dir-face)
          " "))
        (t (concat (all-the-icons-icon-for-file cand) " "))))

(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql project-file)))
  "Return the icon for the candidate CAND of completion category project-file."
  (all-the-icons-completion-get-icon cand 'file))

(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql buffer)))
  "Return the icon for the candidate CAND of completion category buffer."
  (with-current-buffer cand
    (let ((icon (all-the-icons-icon-for-buffer)))
      (concat (if (stringp icon) icon (all-the-icons-faicon "sticky-note-o")) " "))))

(autoload 'bookmark-get-filename "bookmark")
(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql bookmark)))
  "Return the icon for the candidate CAND of completion category bookmark."
  (if-let (fname (bookmark-get-filename cand))
      (all-the-icons-completion-get-icon fname 'file)
    (concat (all-the-icons-octicon "bookmark" :face 'all-the-icons-completion-dir-face) " ")))

(defun all-the-icons-completion-completion-metadata-get (orig metadata prop)
  "Meant as :around advice for `completion-metadata-get', Add icons as prefix.
ORIG should be `completion-metadata-get'
METADATA is the metadata.
PROP is the property which is looked up."
  (if (eq prop 'affixation-function)
      (let ((cat (funcall orig metadata 'category))
            (aff (or (funcall orig metadata 'affixation-function)
                     (when-let ((ann (funcall orig metadata 'annotation-function)))
                       (lambda (cands)
                         (mapcar (lambda (x) (list x "" (funcall ann x))) cands))))))
        (cond
         ((and (eq cat 'multi-category) aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (all-the-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (all-the-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (all-the-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (all-the-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun all-the-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `all-the-icons-completion-mode' to it."
  (all-the-icons-completion-mode (if marginalia-mode 1 -1)))

;;;###autoload
(define-minor-mode all-the-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if all-the-icons-completion-mode
      (advice-add #'completion-metadata-get :around #'all-the-icons-completion-completion-metadata-get)
    (advice-remove #'completion-metadata-get #'all-the-icons-completion-completion-metadata-get)))

(provide 'all-the-icons-completion)
;;; all-the-icons-completion.el ends here
