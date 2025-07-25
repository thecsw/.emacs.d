;;; helm-imenu.el --- Helm interface for Imenu -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2025 Thierry Volpiatto

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'imenu)
(require 'helm-utils)
(require 'helm-help)
(require 'helm-x-icons)

(defvar all-the-icons-default-adjust)
(defvar all-the-icons-scale-factor)
(defvar nerd-icons-default-adjust)
(defvar nerd-icons-scale-factor)

(declare-function which-function "which-func")


(defgroup helm-imenu nil
  "Imenu related libraries and applications for Helm."
  :group 'helm)

(defcustom helm-imenu-delimiter " / "
  "Delimit types of candidates and their value in `helm-buffer'."
  :group 'helm-imenu
  :type 'string)

(defcustom helm-imenu-execute-action-at-once-if-one
  #'helm-imenu--execute-action-at-once-p
  "Goto the candidate when only one is remaining."
  :group 'helm-imenu
  :type 'function)

(defcustom helm-imenu-all-buffer-assoc nil
  "Major mode association alist for `helm-imenu-in-all-buffers'.
Allow `helm-imenu-in-all-buffers' searching in these associated
buffers even if they are not derived from each other.  The alist
is bidirectional, i.e. no need to add \\='((foo . bar) (bar . foo)),
only \\='((foo . bar)) is needed."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'helm-imenu)

(defcustom helm-imenu-in-all-buffers-separate-sources t
  "Display imenu index of each buffer in its own source when non-nil.

When nil all candidates are displayed in a single source.

NOTE: Each source will have as name \"Imenu <buffer-name>\".
`helm-source-imenu-all' will not be set, however it will continue
to be used as a flag for using default as input.  If you do not
want this behavior, remove it from
`helm-sources-using-default-as-input' even if not using a single
source to display imenu in all buffers."
  :type 'boolean
  :group 'helm-imenu)

(defcustom helm-imenu-type-faces
  '(("^Variables$" . font-lock-variable-name-face)
    ("^\\(Function\\|Functions\\|Defuns\\)$" . font-lock-function-name-face)
    ("^\\(Types\\|Provides\\|Requires\\|Classes\\|Class\\|Includes\\|Imports\\|Misc\\|Code\\)$" . font-lock-type-face))
  "Faces for showing type in helm-imenu.
This is a list of cons cells.  The cdr of each cell is a face to
be used, and it can also just be like \\='(:foreground
\"yellow\").  Each car is a regexp match pattern of the imenu type
string."
  :group 'helm-faces
  :type '(repeat
          (cons
           (regexp :tag "Imenu type regexp pattern")
           (sexp :tag "Face"))))

(defcustom helm-imenu-extra-modes nil
  "Extra modes where `helm-imenu-in-all-buffers' should look into."
  :group 'helm-imenu
  :type '(repeat symbol))

(defcustom helm-imenu-hide-item-type-name nil
  "Hide display name of imenu item type along with the icon when non nil.

This value can be toggled with \\<helm-imenu-map>\\[helm-imenu-toggle-type-view].
Don't use `setq' to set this."
  :group 'helm-imenu
  :type 'boolean
  :set (lambda (var val)
         (if (require helm-x-icons-provider nil t)
             (set var val)
           (set var nil))))

(defcustom helm-imenu-use-icon nil
  "Display an icon from `helm-x-icons-provider' package when non nil.

Don't use `setq' to set this."
  :group 'helm-imenu
  :type 'boolean
  :set (lambda (var val)
         (set var (and (require helm-x-icons-provider nil t) val))))

(defcustom helm-imenu-icon-type-alist
  '(("Array"           . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Arrays"          . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Boolean"         . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Booleans"        . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Class"           . (helm-x-icons-generic "package" :face font-lock-type-face))
    ("Classes"         . (helm-x-icons-generic "package" :face font-lock-type-face))
    ("Color"           . (helm-x-icons-generic "color_lens" :face font-lock-builtin-face))
    ("Colors"          . (helm-x-icons-generic "color_lens" :face font-lock-builtin-face))
    ("Constant"        . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Constants"       . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Constructor"     . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Constructors"    . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Enum Member"     . (helm-x-icons-generic "three-bars" :face font-lock-type-face))
    ("Enum Members"    . (helm-x-icons-generic "three-bars" :face font-lock-type-face))
    ("Enum"            . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Enums"           . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Event"           . (helm-x-icons-generic "lightning" :face font-lock-builtin-face))
    ("Events"          . (helm-x-icons-generic "lightning" :face font-lock-builtin-face))
    ("Field"           . (helm-x-icons-generic "three-bars" :face font-lock-type-face))
    ("Fields"          . (helm-x-icons-generic "three-bars" :face font-lock-type-face))
    ("File"            . (helm-x-icons-generic "file" :face font-lock-variable-name-face))
    ("Files"           . (helm-x-icons-generic "file" :face font-lock-variable-name-face))
    ("Folder"          . (helm-x-icons-generic "folder" :face font-lock-variable-name-face))
    ("Folders"         . (helm-x-icons-generic "folder" :face font-lock-variable-name-face))
    ("Interface"       . (helm-x-icons-generic "package" :face font-lock-builtin-face))
    ("Interfaces"      . (helm-x-icons-generic "package" :face font-lock-builtin-face))
    ("Keyword"         . (helm-x-icons-generic "key" :face font-lock-builtin-face))
    ("Keywords"        . (helm-x-icons-generic "key" :face font-lock-builtin-face))
    ("Method"          . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Methods"         . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Defun"           . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Defuns"          . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Fn"              . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Fns"             . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Function"        . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Functions"       . (helm-x-icons-generic "cube" :face font-lock-function-name-face))
    ("Misc"            . (helm-x-icons-generic "globe" :face font-lock-function-name-face))
    ("Miscs"           . (helm-x-icons-generic "globe" :face font-lock-function-name-face))
    ("Module"          . (helm-x-icons-generic "angle-double-right" :face font-lock-builtin-face))
    ("Modules"         . (helm-x-icons-generic "angle-double-right" :face font-lock-builtin-face))
    ("Numeric"         . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Numerics"        . (helm-x-icons-generic "crop" :face font-lock-builtin-face))
    ("Object"          . (helm-x-icons-generic "angle-double-right" :face font-lock-builtin-face))
    ("Objects"         . (helm-x-icons-generic "angle-double-right" :face font-lock-builtin-face))
    ("Operator"        . (helm-x-icons-generic "calculator" :face font-lock-builtin-face))
    ("Operators"       . (helm-x-icons-generic "calculator" :face font-lock-builtin-face))
    ("Property"        . (helm-x-icons-generic "book" :face font-lock-variable-name-face))
    ("Properties"      . (helm-x-icons-generic "book" :face font-lock-variable-name-face))
    ("Reference"       . (helm-x-icons-generic "book" :face font-lock-variable-name-face))
    ("References"      . (helm-x-icons-generic "book" :face font-lock-variable-name-face))
    ("Snippet"         . (helm-x-icons-generic "border_style" :face font-lock-variable-name-face))
    ("Snippets"        . (helm-x-icons-generic "border_style" :face font-lock-variable-name-face))
    ("String"          . (helm-x-icons-generic "text_fields" :face font-lock-variable-name-face))
    ("Strings"         . (helm-x-icons-generic "text_fields" :face font-lock-variable-name-face))
    ("Struct"          . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Structs"         . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Text"            . (helm-x-icons-generic "text_fields" :face font-lock-variable-name-face))
    ("Texts"           . (helm-x-icons-generic "text_fields" :face font-lock-variable-name-face))
    ("Top level"       . (helm-x-icons-generic "globe" :face font-lock-function-name-face))
    ("Trait"           . (helm-x-icons-generic "package" :face font-lock-builtin-face))
    ("Traits"          . (helm-x-icons-generic "package" :face font-lock-builtin-face))
    ("Type"            . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Types"           . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Type Parameter"  . (helm-x-icons-generic "code" :face font-lock-type-face))
    ("Type Parameters" . (helm-x-icons-generic "code" :face font-lock-type-face))
    ("Unit"            . (helm-x-icons-generic "bar-chart" :face font-lock-builtin-face))
    ("Units"           . (helm-x-icons-generic "bar-chart" :face font-lock-builtin-face))
    ("Value"           . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Values"          . (helm-x-icons-generic "cog" :face font-lock-type-face))
    ("Variable"        . (helm-x-icons-generic "book" :face font-lock-variable-name-face))
    ("Variables"       . (helm-x-icons-generic "book":face font-lock-variable-name-face)))
  "An alist of types associated with a sexp returning an icon.
The sexp should be an `all-the-icons' function with its args."
  :type '(alist :key-type string :value-type sexp)
  :group 'helm-imenu)

(defcustom helm-imenu-default-type-sexp
  '(helm-x-icons-generic "globe" :face font-lock-function-name-face)
  "Default sexp to use when no type for an object is found."
  :type 'sexp
  :group 'helm-imenu)

;;; keymap
(defvar helm-imenu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-imenu-next-section)
    (define-key map (kbd "M-<up>")   'helm-imenu-previous-section)
    (define-key map (kbd "C-]") 'helm-imenu-toggle-type-view)
    map))

(defun helm-imenu-toggle-type-view ()
  "Toggle candidate type view."
  (interactive)
  (with-helm-window
    (setq helm-imenu-hide-item-type-name (not helm-imenu-hide-item-type-name))
    (let* ((sel  (substring (helm-get-selection nil 'withprop)
                            (if helm-imenu-use-icon 2 0)))
           (type (get-text-property 1 'type-name sel)))
      (setq sel (substring-no-properties sel))
      (helm-force-update (if helm-imenu-hide-item-type-name
                             (format "^[ ]*%s$"
                                     (car (last (split-string
                                                 sel helm-imenu-delimiter t))))
                           (format "^[ ]*%s / %s$"
                                   type sel))))))
(put 'helm-imenu-toggle-type-view 'no-helm-mx t)

(defcustom helm-imenu-lynx-style-map nil
  "Use Arrow keys to jump to occurences."
  :group 'helm-imenu
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (if val
             (progn
               (define-key helm-imenu-map (kbd "<right>")  'helm-execute-persistent-action)
               (define-key helm-imenu-map (kbd "<left>")   'helm-maybe-exit-minibuffer))
           (define-key helm-imenu-map (kbd "<right>") nil)
           (define-key helm-imenu-map (kbd "<left>")  nil))))

(defun helm-imenu-next-or-previous-section (n)
  (with-helm-window
    (let* ((fn (lambda ()
                 (let ((str (buffer-substring
                             (pos-bol) (pos-eol))))
                   (if helm-imenu-hide-item-type-name
                       (get-text-property 1 'type-name str)
                   (car (split-string str helm-imenu-delimiter))))))
           (curtype (funcall fn))
           (stop-fn (if (> n 0)
                        #'helm-end-of-source-p
                      #'helm-beginning-of-source-p)))
      (while (and (not (funcall stop-fn))
                  (string= curtype (funcall fn)))
        (forward-line n))
      (helm-mark-current-line)
      (helm-follow-execute-persistent-action-maybe))))

(defun helm-imenu-next-section ()
  (interactive)
  (helm-imenu-next-or-previous-section 1))

(defun helm-imenu-previous-section ()
  (interactive)
  (helm-imenu-next-or-previous-section -1))


;;; Internals
(defvar helm-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-cached-imenu-alist)

(defvar helm-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-cached-imenu-candidates)

(defvar helm-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-cached-imenu-tick)

(defvar helm-imenu--in-all-buffers-cache nil)


(defvar helm-source-imenu nil "See (info \"(emacs)Imenu\")")
(defvar helm-source-imenu-all nil)

(defclass helm-imenu-source (helm-source-sync)
  ((candidates :initform 'helm-imenu-candidates)
   (candidate-transformer :initform 'helm-imenu-transformer)
   (persistent-action :initform 'helm-imenu-persistent-action)
   (persistent-help :initform "Show this entry")
   (nomark :initform t)
   (keymap :initform 'helm-imenu-map)
   (help-message :initform 'helm-imenu-help-message)
   (action :initform 'helm-imenu-action)
   (find-file-target :initform #'helm-imenu-quit-and-find-file-fn)
   (group :initform 'helm-imenu)))

(defcustom helm-imenu-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-imenu'."
  :group 'helm-imenu
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-imenu
               (helm-make-source "Imenu" 'helm-imenu-source
                 :fuzzy-match helm-imenu-fuzzy-match))))

(defun helm-imenu--maybe-switch-to-buffer (candidate)
  (let ((cand (cdr candidate)))
    (helm-aif (and (markerp cand) (marker-buffer cand))
        (helm-buffers-switch-to-buffer-or-tab it))))

(defun helm-imenu--execute-action-at-once-p ()
  (let ((cur (helm-get-selection))
        (mb (with-helm-current-buffer
              (save-excursion
                (goto-char (pos-bol))
                 (point-marker)))))
    ;; Happen when cursor is on the line where a definition is. This
    ;; prevent jumping to the definition where we are already, instead
    ;; display helm with all definitions and preselection to the place
    ;; we already are.
    (if (equal (cdr cur) mb)
        (prog1 nil
          (helm-set-pattern "")
          (helm-force-update
           (concat "\\_<" (regexp-quote (car cur)) "\\_>")))
        t)))

(defun helm-imenu-quit-and-find-file-fn (source)
  (let ((sel (helm-get-selection nil nil source)))
    (when (and (consp sel) (markerp (cdr sel)))
      (buffer-file-name (marker-buffer (cdr sel))))))

(defun helm-imenu-action (candidate)
  "Default action for `helm-source-imenu'."
  (helm-log-run-hook "helm-imenu-action" 'helm-goto-line-before-hook)
  (helm-imenu--maybe-switch-to-buffer candidate)
  (imenu candidate)
  ;; If semantic is supported in this buffer
  ;; imenu used `semantic-imenu-goto-function'
  ;; and position have been highlighted,
  ;; no need to highlight again.
  (unless (eq imenu-default-goto-function
              'semantic-imenu-goto-function)
    (helm-highlight-current-line)))

(defun helm-imenu-persistent-action (candidate)
  "Default persistent action for `helm-source-imenu'."
  (helm-imenu--maybe-switch-to-buffer candidate)
  (imenu candidate)
  (helm-highlight-current-line))

(defun helm-imenu-candidates (&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
        (setq imenu--index-alist nil)
        (prog1 (setq helm-cached-imenu-candidates
                     (let ((index (imenu--make-index-alist t)))
                       (helm-imenu--candidates-1
                        (delete (assoc "*Rescan*" index) index))))
          (setq helm-cached-imenu-tick tick))))))

(defun helm-imenu-candidates-in-all-buffers (&optional build-sources)
  (let* ((lst (buffer-list))
         (progress-reporter (make-progress-reporter
                             "Imenu indexing buffers..." 1 (length lst))))
    (prog1
        (cl-loop with cur-buf = (if build-sources
                                    (current-buffer) helm-current-buffer)
                 for b in lst
                 for count from 1
                 when (with-current-buffer b
                        (and (or (member major-mode helm-imenu-extra-modes)
                                 (derived-mode-p 'prog-mode))
                             (helm-same-major-mode-p
                              cur-buf helm-imenu-all-buffer-assoc)))
                 if build-sources
                 collect (helm-make-source
                             (format "Imenu in %s" (buffer-name b))
                             'helm-imenu-source
                           :candidates (with-current-buffer b
                                         (helm-imenu-candidates b))
                           :fuzzy-match helm-imenu-fuzzy-match)
                 else
                 append (with-current-buffer b
                          (helm-imenu-candidates b))
                 do (progress-reporter-update progress-reporter count))
      (progress-reporter-done progress-reporter))))

(defun helm-imenu--candidates-1 (alist)
  (cl-loop for elm in alist
           nconc (cond
                  ((imenu--subalist-p elm)
                   (helm-imenu--candidates-1
                    (cl-loop for (e . v) in (cdr elm) collect
                             (cons (propertize
                                    e 'helm-imenu-type (car elm))
                                   ;; If value is an integer, convert it
                                   ;; to a marker, otherwise it is a cons cell
                                   ;; and it will be converted on next recursions.
                                   ;; (Bug#1060) [1].
                                   (if (integerp v) (copy-marker v) v)))))
                  ((listp (cdr elm))
                   (and elm (list elm)))
                  (t
                   ;; bug in imenu, should not be needed.
                   (and (cdr elm)
                        ;; Semantic uses overlays whereas imenu uses
                        ;; markers (Bug#1706).
                        (setcdr elm (helm-acase (cdr elm) ; Same as [1].
                                      ((guard* (overlayp it))
                                       (copy-overlay it))
                                      ((guard* (or (markerp it) (integerp it)))
                                       (copy-marker it))))
                        (list elm))))))

(defun helm-imenu--get-prop (item)
  ;; property value of ITEM can have itself
  ;; a property value which have itself a property value
  ;; ...and so on; Return a list of all these
  ;; properties values starting at ITEM.
  (let* ((prop (get-text-property 0 'helm-imenu-type item))
         (lst  (list prop item)))
    (when prop
      (while prop
        (setq prop (get-text-property 0 'helm-imenu-type prop))
        (and prop (push prop lst)))
      lst)))

(defun helm-imenu-icon-for-type (type)
  "Return an icon for type TYPE.
The icon is found in `helm-imenu-icon-type-alist', if not
`helm-imenu-default-type-sexp' is evaled to provide a default icon."
  (let ((all-the-icons-scale-factor 1.0)
        (all-the-icons-default-adjust 0.0)
        (nerd-icons-scale-factor 1.0)
        (nerd-icons-default-adjust 0.0))
    (or (helm-aand (assoc-default type helm-imenu-icon-type-alist)
                   (apply (car it) (cdr it)))
        (apply (car helm-imenu-default-type-sexp)
               (cdr helm-imenu-default-type-sexp)))))

(defun helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates
           ;; (k . v) == (symbol-name . marker)
           for bufname = (buffer-name
                          (helm-acase v
                            ((guard* (overlayp it)) (overlay-buffer it))
                            ((guard* (markerp it)) (marker-buffer it))))
           for types = (or (helm-imenu--get-prop k)
                           (list (if (with-current-buffer bufname
                                       (derived-mode-p 'prog-mode))
                                     "Function"
                                   "Top level")
                                 k))
           for type-icon = (and helm-imenu-use-icon
                                (helm-imenu-icon-for-type (car types)))
           for type-name = (propertize
                            (car types) 'face
                            (cl-loop for (p . f) in helm-imenu-type-faces
                                     when (string-match p (car types))
                                     return f
                                     finally return 'default))
           for disp1 = (mapconcat 'identity
                                  (cdr types)
                                  (propertize helm-imenu-delimiter
                                              'face 'shadow))
           for disp = (concat (if helm-imenu-use-icon
                                  (concat (propertize " " 'display type-icon) " ")
                                "")
                              (if helm-imenu-hide-item-type-name
                                  ""
                                (concat type-name
                                        (propertize helm-imenu-delimiter
                                                    'face 'shadow)))
                              (propertize disp1 'help-echo bufname 'types types))
           collect
           (cons (propertize disp 'type-name type-name) (cons k v))))


;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (require 'which-func)
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let* ((imenu-auto-rescan t)
         (helm-highlight-matches-around-point-max-lines 'never)
         (str (thing-at-point 'symbol))
         (init-reg (and str (concat "\\_<" (regexp-quote str) "\\_>")))
         (helm-execute-action-at-once-if-one
          helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'helm-source-imenu
          :default (and str (list init-reg str))
          :preselect (helm-aif (which-function)
                         (concat "\\_<" (regexp-quote it) "\\_>")
                       init-reg)
          :buffer "*helm imenu*")))

;;;###autoload
(defun helm-imenu-in-all-buffers ()
  "Fetch Imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived
i.e. `derived-mode-p' or it have an association in
`helm-imenu-all-buffer-assoc'."
  (interactive)
  (require 'which-func)
  (unless helm-imenu-in-all-buffers-separate-sources
    (unless helm-source-imenu-all
      (setq helm-source-imenu-all
            (helm-make-source "Imenu in all buffers" 'helm-imenu-source
              :init (lambda ()
                      ;; Use a cache to avoid repeatedly sending
                      ;; progress-reporter message when updating
                      ;; (Bug#1704).
                      (setq helm-imenu--in-all-buffers-cache
                            (helm-imenu-candidates-in-all-buffers)))
              :candidates 'helm-imenu--in-all-buffers-cache
              :fuzzy-match helm-imenu-fuzzy-match))))
  (let* ((imenu-auto-rescan t)
         (helm-highlight-matches-around-point-max-lines 'never)
         (str (thing-at-point 'symbol))
         (init-reg (and str (concat "\\_<" (regexp-quote str) "\\_>")))
         (helm-execute-action-at-once-if-one
          helm-imenu-execute-action-at-once-if-one)
         (helm-maybe-use-default-as-input
          (not (null (memq 'helm-source-imenu-all
                           helm-sources-using-default-as-input))))
         (sources (if helm-imenu-in-all-buffers-separate-sources
                      (helm-imenu-candidates-in-all-buffers 'build-sources)
                    '(helm-source-imenu-all))))
    (helm :sources sources
          :default (and str (list init-reg str))
          :preselect (helm-aif (which-function)
                         (concat "\\_<" (regexp-quote it) "\\_>")
                       init-reg)
          :buffer "*helm imenu all*")))

(provide 'helm-imenu)

;;; helm-imenu.el ends here
