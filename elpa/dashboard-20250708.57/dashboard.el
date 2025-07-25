;;; dashboard.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2025 emacs-dashboard maintainers
;;
;; Author     : Rakan Al-Hneiti <rakan.alhneiti@gmail.com>
;; Maintainer : Shen, Jen-Chieh <jcs090218@gmail.com>
;;              Ricardo Arredondo <ricardo.richo@gmail.com>
;; URL        : https://github.com/emacs-dashboard/emacs-dashboard
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: October 05, 2016
;; Package-Version: 20250708.57
;; Package-Revision: 8c2cf0cfde4f
;; Keywords: startup, screen, tools, dashboard
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projects (projectile or project.el), org-agenda and more.

;;; Code:

(require 'ffap)
(require 'recentf)

(require 'dashboard-widgets)

;;
;;; Externals

(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function dashboard-ls--dirs "ext:dashboard-ls.el")
(declare-function dashboard-ls--files "ext:dashboard-ls.el")
(declare-function page-break-lines-mode "ext:page-break-lines.el")
(declare-function projectile-remove-known-project "ext:projectile.el")
(declare-function project-forget-projects-under "ext:project.el")
(declare-function linum-mode "linum.el")

(declare-function dashboard-refresh-buffer "dashboard.el")

;;
;;; Customization

(defgroup dashboard nil
  "Extensible startup screen."
  :group 'applications)

;; Custom splash screen
(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-p") #'dashboard-previous-line)
    (define-key map (kbd "C-n") #'dashboard-next-line)
    (define-key map (kbd "<up>") #'dashboard-previous-line)
    (define-key map (kbd "<down>") #'dashboard-next-line)
    (define-key map (kbd "k") #'dashboard-previous-line)
    (define-key map (kbd "j") #'dashboard-next-line)
    (define-key map [tab] #'widget-forward)
    (define-key map (kbd "C-i") #'widget-forward)
    (define-key map [backtab] #'widget-backward)
    (define-key map (kbd "RET") #'dashboard-return)
    (define-key map (kbd "<touchscreen-begin>") #'widget-button-click)
    (define-key map [mouse-1] #'dashboard-mouse-1)
    (define-key map (kbd "}") #'dashboard-next-section)
    (define-key map (kbd "{") #'dashboard-previous-section)

    (define-key map (kbd "<backspace>") #'dashboard-remove-item-under)
    (define-key map (kbd "<delete>") #'dashboard-remove-item-under)
    (define-key map (kbd "DEL") #'dashboard-remove-item-under)

    (define-key map (kbd "1") #'dashboard-section-1)
    (define-key map (kbd "2") #'dashboard-section-2)
    (define-key map (kbd "3") #'dashboard-section-3)
    (define-key map (kbd "4") #'dashboard-section-4)
    (define-key map (kbd "5") #'dashboard-section-5)
    (define-key map (kbd "6") #'dashboard-section-6)
    (define-key map (kbd "7") #'dashboard-section-7)
    (define-key map (kbd "8") #'dashboard-section-8)
    (define-key map (kbd "9") #'dashboard-section-9)
    map)
  "Keymap for dashboard mode.")

(defcustom dashboard-before-initialize-hook nil
  "Hook that is run before dashboard buffer is initialized."
  :group 'dashboard
  :type 'hook)

(defcustom dashboard-after-initialize-hook nil
  "Hook that is run after dashboard buffer is initialized."
  :group 'dashboard
  :type 'hook)

(defcustom dashboard-hide-cursor nil
  "Whether to hide the cursor in the dashboard."
  :type 'boolean
  :group 'dashboard)

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Dashboard major mode for startup screen."
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (when (featurep 'whitespace) (whitespace-mode -1))
  (when (featurep 'linum) (linum-mode -1))
  (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
  (when (featurep 'page-break-lines) (page-break-lines-mode 1))
  (setq-local revert-buffer-function #'dashboard-refresh-buffer)
  (when dashboard-hide-cursor
    (setq-local cursor-type nil))
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

(defcustom dashboard-center-content nil
  "Whether to center content within the window."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-vertically-center-content nil
  "Whether to vertically center content within the window."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-startupify-list
  '(dashboard-insert-banner
    dashboard-insert-newline
    dashboard-insert-banner-title
    dashboard-insert-newline
    dashboard-insert-init-info
    dashboard-insert-items
    dashboard-insert-newline
    dashboard-insert-footer)
  "List of dashboard widgets (in order) to insert in dashboard buffer.
Avalaible functions:
  `dashboard-insert-newline'
  `dashboard-insert-page-break'
  `dashboard-insert-banner'
  `dashboard-insert-banner-title'
  `dashboard-insert-navigator'
  `dashboard-insert-init-info'
  `dashboard-insert-items'
  `dashboard-insert-footer'

It must be a function or a cons cell where specify function and
its arg.

Also you can add your custom function or a lambda to the list.
example:
 (lambda () (delete-char -1))"
  :type '(repeat (choice
                  function
                  (cons function sexp)))
  :group 'dashboard)

(defcustom dashboard-navigation-cycle nil
  "Non-nil cycle the section navigation."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-buffer-name "*dashboard*"
  "Dashboard's buffer name."
  :type 'string
  :group 'dashboard)

(defvar dashboard--section-starts nil
  "List of section starting positions.")

;;
;;; Util

(defun dashboard--goto-line (line)
  "Goto LINE."
  (goto-char (point-min)) (forward-line (1- line)))

(defmacro dashboard--save-excursion (&rest body)
  "Execute BODY save window point."
  (declare (indent 0) (debug t))
  `(let ((line (line-number-at-pos nil t))
         (column (current-column)))
     ,@body
     (dashboard--goto-line line)
     (move-to-column column)))

;;
;;; Core

(defun dashboard--separator ()
  "Return separator used to search."
  (concat "\n" dashboard-page-separator))

(defun dashboard--current-section ()
  "Return section symbol in dashboard."
  (save-excursion
    (if-let* ((sep (dashboard--separator))
              ((and (search-backward sep nil t)
                    (search-forward sep nil t)))
              (ln (thing-at-point 'line t)))
        (cond ((string-match-p "Recent Files:" ln)     'recents)
              ((string-match-p "Bookmarks:" ln)        'bookmarks)
              ((string-match-p "Projects:" ln)         'projects)
              ((string-match-p "Agenda for " ln)       'agenda)
              ((string-match-p "Registers:" ln)        'registers)
              ((string-match-p "List Directories:" ln) 'ls-directories)
              ((string-match-p "List Files:" ln)       'ls-files)
              (t (user-error "Unknown section from dashboard")))
      (user-error "Failed searching dashboard section"))))

;;
;;; Navigation

(defun dashboard-previous-section ()
  "Navigate backwards to previous section."
  (interactive)
  (let* ((items-len (1- (length dashboard-items)))
         (first-item (car (nth 0 dashboard-items)))
         (current (or (ignore-errors (dashboard--current-section))
                      first-item))
         (items (mapcar #'car dashboard-items))
         (find (cl-position current items :test #'equal))
         (prev-index (1- find))
         (prev (cond (dashboard-navigation-cycle
                      (if (< prev-index 0) (nth items-len items)
                        (nth prev-index items)))
                     (t
                      (if (< prev-index 0) (nth 0 items)
                        (nth prev-index items))))))
    (dashboard--goto-section prev)))

(defun dashboard-next-section ()
  "Navigate forward to next section."
  (interactive)
  (let* ((items-len (1- (length dashboard-items)))
         (last-item (car (nth items-len dashboard-items)))
         (current (or (ignore-errors (dashboard--current-section))
                      last-item))
         (items (mapcar #'car dashboard-items))
         (find (cl-position current items :test #'equal))
         (next-index (1+ find))
         (next (cond (dashboard-navigation-cycle
                      (or (nth next-index items)
                          (nth 0 items)))
                     (t
                      (if (< items-len next-index)
                          (nth (min items-len next-index) items)
                        (nth next-index items))))))
    (dashboard--goto-section next)))

(defun dashboard--section-lines ()
  "Return a list of integer represent the starting line number of each section."
  (let (pb-lst)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (dashboard--separator) nil t)
        (when (ignore-errors (dashboard--current-section))
          (push (line-number-at-pos) pb-lst))))
    (setq pb-lst (reverse pb-lst))
    pb-lst))

(defun dashboard--goto-section-by-index (index)
  "Navigate to item section by INDEX."
  (let* ((pg-lst (dashboard--section-lines))
         (items-id (1- index))
         (items-pg (nth items-id pg-lst))
         (items-len (length pg-lst)))
    (when (and items-pg (< items-id items-len))
      (dashboard--goto-line items-pg))))

(defun dashboard-cycle-section-forward (&optional section)
  "Cycle forward through the entries in SECTION.
If SECTION is nil, cycle in the current section."
  (let ((target-section (or section (dashboard--current-section))))
    (if target-section
        (condition-case nil
            (progn
              (widget-forward 1)
              (unless (eq target-section (dashboard--current-section))
                (dashboard--goto-section target-section)))
          (widget-forward 1))
      (widget-forward 1))))

(defun dashboard-cycle-section-backward (&optional section)
  "Cycle backward through the entries in SECTION.
If SECTION is nil, cycle in the current section."
  (let ((target-section (or section (dashboard--current-section))))
    (if target-section
        (condition-case nil
            (progn
              (widget-backward 1)
              (unless (eq target-section (dashboard--current-section))
                (progn
                  (dashboard--goto-section target-section)
                  (while (eq target-section (dashboard--current-section))
                    (widget-forward 1))
                  (widget-backward 1))))
          (widget-backward 1))
      (widget-backward 1))))

(defun dashboard-section-1 ()
  "Navigate to section 1." (interactive) (dashboard--goto-section-by-index 1))
(defun dashboard-section-2 ()
  "Navigate to section 2." (interactive) (dashboard--goto-section-by-index 2))
(defun dashboard-section-3 ()
  "Navigate to section 3." (interactive) (dashboard--goto-section-by-index 3))
(defun dashboard-section-4 ()
  "Navigate to section 4." (interactive) (dashboard--goto-section-by-index 4))
(defun dashboard-section-5 ()
  "Navigate to section 5." (interactive) (dashboard--goto-section-by-index 5))
(defun dashboard-section-6 ()
  "Navigate to section 6." (interactive) (dashboard--goto-section-by-index 6))
(defun dashboard-section-7 ()
  "Navigate to section 7." (interactive) (dashboard--goto-section-by-index 7))
(defun dashboard-section-8 ()
  "Navigate to section 8." (interactive) (dashboard--goto-section-by-index 8))
(defun dashboard-section-9 ()
  "Navigate to section 9." (interactive) (dashboard--goto-section-by-index 9))

(defun dashboard-previous-line (arg)
  "Move point up and position it at that line’s item.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (dashboard-next-line (- arg)))

(defun dashboard-next-line (arg)
  "Move point down and position it at that line’s item.
Optional prefix ARG says how many lines to move; default is one line."
  ;; code heavily inspired by `dired-next-line'
  (interactive "^p")
  (let (line-move-visual goal-column)
    (line-move arg t))
  ;; We never want to move point into an invisible line.  Dashboard doesn’t
  ;; use invisible text currently but when it does we’re ready!
  (while (and (invisible-p (point))
              (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (beginning-of-line-text))

;;
;;; ffap

(defun dashboard--goto-section (section)
  "Move to SECTION declares in variable `dashboard-item-shortcuts'."
  (let ((fnc (intern (format "dashboard-jump-to-%s" section))))
    (dashboard-funcall-fboundp fnc)))

(defun dashboard--current-index (section &optional pos)
  "Return the idex by SECTION from POS."
  (let (target-ln section-line)
    (save-excursion
      (when pos (goto-char pos))
      (setq target-ln (line-number-at-pos))
      (dashboard--goto-section section)
      (setq section-line (line-number-at-pos)))
    (- target-ln section-line)))

(defun dashboard--section-list (section)
  "Return the list from SECTION."
  (cl-case section
    (`recents        recentf-list)
    (`bookmarks      (bookmark-all-names))
    (`projects       (dashboard-projects-backend-load-projects))
    (`ls-directories (dashboard-ls--dirs))
    (`ls-files       (dashboard-ls--files))
    (t (user-error "Unknown section for search: %s" section))))

(defun dashboard--current-item-in-path ()
  "Return the path from current dashboard section in path."
  (let ((section (dashboard--current-section)) path)
    (cl-case section
      (`bookmarks (setq path (bookmark-get-filename path)))
      (t
       (let ((lst (dashboard--section-list section))
             (index (dashboard--current-index section)))
         (setq path (nth index lst)))))
    path))

(defun dashboard--on-path-item-p ()
  "Return non-nil if current point is on the item path from dashboard."
  (save-excursion
    (when (= (point) (line-end-position)) (ignore-errors (forward-char -1)))
    (eq (get-char-property (point) 'face) 'dashboard-items-face)))

(defun dashboard--ffap-guesser--adv (fnc &rest args)
  "Advice execution around function `ffap-guesser'.

Argument FNC is the adviced function.
Optional argument ARGS adviced function arguments."
  (cl-case major-mode
    (`dashboard-mode
     (or (and (dashboard--on-path-item-p)
              (dashboard--current-item-in-path))
         (apply fnc args)))  ; fallback
    (t (apply fnc args))))
(advice-add 'ffap-guesser :around #'dashboard--ffap-guesser--adv)

;;
;;; Removal

(defun dashboard-remove-item-under ()
  "Remove a item from the current item section."
  (interactive)
  (cl-case (dashboard--current-section)
    (`recents   (dashboard-remove-item-recentf))
    (`bookmarks (dashboard-remove-item-bookmarks))
    (`projects  (dashboard-remove-item-projects))
    (`agenda    (dashboard-remove-item-agenda))
    (`registers (dashboard-remove-item-registers)))
  (dashboard--save-excursion (dashboard-refresh-buffer)))

(defun dashboard-remove-item-recentf ()
  "Remove a file from `recentf-list'."
  (interactive)
  (let ((path (save-excursion (end-of-line) (ffap-guesser))))
    (setq recentf-list (delete path recentf-list)))
  (dashboard-mute-apply (recentf-save-list)))

(defun dashboard-remove-item-projects ()
  "Remove a path from `project--list'."
  (interactive)
  (let ((path (save-excursion (end-of-line) (ffap-guesser))))
    (dashboard-mute-apply
      (cl-case dashboard-projects-backend
        (`projectile (projectile-remove-known-project path))
        (`project-el (project-forget-projects-under path))))))

(defun dashboard-remove-item-bookmarks ()
  "Remove a bookmarks from `bookmark-alist'."
  (interactive))  ; TODO: ..

(defun dashboard-remove-item-agenda ()
  "Remove an agenda from `org-agenda-files'."
  (interactive "P")
  (let ((agenda-file (get-text-property (point) 'dashboard-agenda-file))
        (agenda-loc (get-text-property (point) 'dashboard-agenda-loc)))
    (with-current-buffer (find-file-noselect agenda-file)
      (goto-char agenda-loc)
      (call-interactively 'org-todo))))

(defun dashboard-remove-item-registers ()
  "Remove a registers from `register-alist'."
  (interactive))  ; TODO: ..

;;
;;; Confirmation

(defun dashboard-return ()
  "Hit return key in dashboard buffer."
  (interactive)
  (let ((start-ln (line-number-at-pos)) (fd-cnt 0) diff-line entry-pt)
    (save-excursion
      (while (and (not diff-line)
                  (not (= (point) (point-min)))
                  (not (get-char-property (point) 'button))
                  (not (= (point) (point-max))))
        (forward-char 1)
        (setq fd-cnt (1+ fd-cnt))
        (unless (= start-ln (line-number-at-pos))
          (setq diff-line t)))
      (unless (= (point) (point-max))
        (setq entry-pt (point))))
    (when (= fd-cnt 1)
      (setq entry-pt (1- (point))))
    (if entry-pt
        (widget-button-press entry-pt)
      (call-interactively #'widget-button-press))))

(defun dashboard-mouse-1 ()
  "Key for keymap `mouse-1'."
  (interactive)
  (let ((old-track-mouse track-mouse))
    (when (call-interactively #'widget-button-click)
      (setq track-mouse old-track-mouse))))

;;
;;; Insertion

(defmacro dashboard--with-buffer (&rest body)
  "Execute BODY in dashboard buffer."
  (declare (indent 0))
  `(with-current-buffer (get-buffer-create dashboard-buffer-name)
     (let ((inhibit-read-only t)) ,@body)
     (current-buffer)))

(defun dashboard-insert-items ()
  "Function to insert dashboard items.
See `dashboard-item-generators' for all items available."
  (let ((recentf-is-on (recentf-enabled-p))
        (origial-recentf-list recentf-list))
    (mapc (lambda (els)
            (let* ((el (or (car-safe els) els))
                   (list-size
                    (or (cdr-safe els)
                        dashboard-items-default-length))
                   (item-generator
                    (cdr-safe (assoc el dashboard-item-generators))))

              (insert "\n")
              (push (point) dashboard--section-starts)
              (funcall item-generator list-size)
              (goto-char (point-max))

              (when recentf-is-on
                (setq recentf-list origial-recentf-list))))
          dashboard-items)

    (when dashboard-center-content
      (dashboard-center-text
       (if dashboard--section-starts
           (car (last dashboard--section-starts))
         (point))
       (point-max)))

    (save-excursion
      (dolist (start dashboard--section-starts)
        (goto-char start)
        (insert dashboard-page-separator)))

    (insert "\n")
    (insert dashboard-page-separator)))

(defun dashboard-insert-startupify-lists (&optional force-refresh)
  "Insert the list of widgets into the buffer, FORCE-REFRESH is optional."
  (interactive)
  (let ((inhibit-redisplay t)
        (recentf-is-on (recentf-enabled-p))
        (origial-recentf-list recentf-list)
        (dashboard-num-recents (or (cdr (assoc 'recents dashboard-items)) 0)))
    (when recentf-is-on
      (setq recentf-list (dashboard-subseq recentf-list dashboard-num-recents)))
    (dashboard--with-buffer
      (when (or force-refresh (not (eq major-mode 'dashboard-mode)))
        (run-hooks 'dashboard-before-initialize-hook)
        (erase-buffer)
        (setq dashboard--section-starts nil)

        (mapc (lambda (entry)
                (if (and (listp entry)
                         (not (functionp entry)))
                    (apply (car entry) `(,(cdr entry)))
                  (funcall entry)))
              dashboard-startupify-list)
        (dashboard-vertically-center)
        (dashboard-mode)))
    (when recentf-is-on
      (setq recentf-list origial-recentf-list))))

(defun dashboard-vertically-center ()
  "Center vertically the content of dashboard.  Always go to point-min char."
  (when-let* (dashboard-vertically-center-content
              (start-height (cdr (window-absolute-pixel-position (point-min))))
              (end-height (cdr (window-absolute-pixel-position (point-max))))
              (content-height (- end-height start-height))
              (vertical-padding (floor (/ (- (window-pixel-height) content-height) 2)))
              ((> vertical-padding 0))
              (vertical-lines (1- (floor (/ vertical-padding (line-pixel-height)))))
              ((> vertical-lines 0)))
    (goto-char (point-min))
    (insert (make-string vertical-lines ?\n)))
  (goto-char (point-min)))

;;;###autoload
(defun dashboard-open (&rest _)
  "Open (or refresh) the *dashboard* buffer."
  (interactive)
  (dashboard--with-buffer
    (switch-to-buffer (current-buffer))
    (dashboard-insert-startupify-lists t)))

(defalias #'dashboard-refresh-buffer #'dashboard-open)

(defun dashboard-resize-on-hook (&optional _)
  "Re-render dashboard on window size change."
  (let ((space-win (get-buffer-window dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (dashboard-insert-startupify-lists)))))

(defun dashboard-initialize ()
  "Switch to dashboard and run `dashboard-after-initialize-hook'."
  (switch-to-buffer dashboard-buffer-name)
  (goto-char (point-min))
  (redisplay)
  (run-hooks 'dashboard-after-initialize-hook))

;;;###autoload
(defun dashboard-setup-startup-hook ()
  "Setup post initialization hooks unless a command line argument is provided."
  (when (< (length command-line-args) 2) ;; Assume no file name passed
    (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
    (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
    (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
    (add-hook 'emacs-startup-hook #'dashboard-initialize)))

(provide 'dashboard)
;;; dashboard.el ends here
