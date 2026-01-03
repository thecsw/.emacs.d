;;; magit-commit-mark.el --- Support marking commits as read -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2021  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-magit-commit-mark
;; Package-Version: 20251229.627
;; Package-Revision: ff967ea9f2fb
;; Package-Requires: ((emacs "29.1") (magit "3.3.0"))

;;; Commentary:

;; Supports keeping track of read SHA1 commits using data stored persistently
;; (between sessions), as well as functionality to toggle read/unread state.

;;; Usage:

;; See readme.rst.

;;; Code:

(require 'magit-diff)
;; For `magit-log-maybe-update-revision-buffer'.
(require 'magit-log)
;; For `url-hexify-string'.
(require 'url-util)


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup magit-commit-mark nil
  "Support marking commits in `magit-log' as read (storing the state persistently)."
  :group 'magit)

(defcustom magit-commit-mark-on-show-commit t
  "Mark commits as read when displayed (with `magit-show-commit')."
  :type 'boolean)

(defcustom magit-commit-mark-on-skip-to-unread nil
  "Mark commits as read immediately when skipping to the next/previous unread."
  :type 'boolean)

(defcustom magit-commit-mark-on-show-commit-delay 2.0
  "Delay (in seconds) before marking as read."
  :type 'float)

(defcustom magit-commit-mark-sha1-length 12
  "The number of SHA1 characters to store and use to identify commits.

This must not be longer than the value used when displaying the log."
  :type 'integer)

(defcustom magit-commit-mark-directory
  (locate-user-emacs-file "magit-commit-mark" ".emacs-magit-commit-mark")
  "The directory to store the repository marking data."
  :type 'directory)


;; ---------------------------------------------------------------------------
;; Faces

(defface magit-commit-mark-read-face '((t :inherit font-lock-comment-face))
  "Face used to highlight the commit as read.")

(defface magit-commit-mark-unread-face '((t :inherit success))
  "Face used to highlight the commit as unread.")

(defface magit-commit-mark-star-face '((t :inherit warning))
  "Face used to highlight the commit as starred.")

(defface magit-commit-mark-urgent-face '((t :inherit error :extend t))
  "Face used to highlight the commit as urgent.")


;; ---------------------------------------------------------------------------
;; Internal Variables

;; An `alist' of hash tables where the key is the repository directory name
;; and the value is a hash table of SHA1 flags for that repository,
;; lazily initialized as needed.
(defvar magit-commit-mark--repo-hashes nil)

(defvar magit-commit-mark--on-show-commit-global-timer nil)

;; Buffer-local overlays to display read/unread status.
(defvar-local magit-commit-mark--overlays nil)

;; Bit indices used for flags in the value of hash tables stored in
;; `magit-commit-mark--repo-hashes' (add more as needed).
(defconst magit-commit-mark--bitflag-read 0)
(defconst magit-commit-mark--bitflag-star 1)
(defconst magit-commit-mark--bitflag-urgent 2)


;; ---------------------------------------------------------------------------
;; Internal Utility Functions

(defun magit-commit-mark--make-file-name-from-repo (repo-name)
  "Return the data file path for REPO-NAME."
  (declare (important-return-value t))
  (file-name-concat magit-commit-mark-directory
                    (concat
                     (url-hexify-string
                      (convert-standard-filename (expand-file-name repo-name)))
                     ".data")))

(defun magit-commit-mark--get-repo-dir ()
  "Return the current repository directory name."
  (declare (important-return-value t))
  (let ((repo-dir (magit-toplevel)))
    (when repo-dir
      ;; Extract directory name only.
      (file-name-nondirectory (file-name-as-directory (expand-file-name repo-dir))))))

(defun magit-commit-mark--get-repo-dir-or-error ()
  "Return the name of the current repository root directory or raise an error."
  (declare (important-return-value t))
  (let ((repo-dir (magit-commit-mark--get-repo-dir)))
    (unless repo-dir
      (user-error "Unable to find the directory for this GIT repository!"))
    repo-dir))

(defun magit-commit-mark--get-sha1-at-point-or-nil ()
  "Return the SHA1 at point or nil."
  (declare (important-return-value t))
  (let ((sha1
         (or (magit-commit-at-point)
             (magit-section-value-if 'module-commit)
             (thing-at-point 'git-revision t))))
    (when sha1
      (substring sha1 0 (min (length sha1) magit-commit-mark-sha1-length)))))

(defun magit-commit-mark--get-sha1-at-point-or-error ()
  "Return the SHA1 at point or raise an error."
  (declare (important-return-value t))
  (let ((sha1 (magit-commit-mark--get-sha1-at-point-or-nil)))
    (unless sha1
      (user-error "No SHA1 found at point"))
    sha1))

(defun magit-commit-mark--get-context-vars-or-error ()
  "Return the repository directory name and SHA1 from the current context (or error)."
  (declare (important-return-value t))
  (let ((repo-dir (magit-commit-mark--get-repo-dir)))
    (let ((sha1 (magit-commit-mark--get-sha1-at-point-or-error)))
      (cons repo-dir sha1))))


(defun magit-commit-mark--region-range-or-all ()
  "Return the region range or buffer range (expanded to line bounds)."
  (declare (important-return-value t))
  (cond
   ((region-active-p)
    (save-excursion
      (cons
       (progn
         (goto-char (region-beginning))
         (pos-bol))
       (progn
         (goto-char (region-end))
         (pos-eol)))))
   (t
    (cons (point-min) (point-max)))))


;; ---------------------------------------------------------------------------
;; Overlay Management

(defun magit-commit-mark--overlay-clear ()
  "Clear all commit mark overlays."
  (declare (important-return-value nil))
  (mapc #'delete-overlay magit-commit-mark--overlays)
  (setq magit-commit-mark--overlays nil))

(defun magit-commit-mark--overlay-refresh-range (repo-hash point-beg point-end)
  "Refresh commit mark overlays between POINT-BEG and POINT-END using REPO-HASH."
  (declare (important-return-value nil))

  (when magit-commit-mark--overlays
    (remove-overlays point-beg point-end 'magit-commit-mark t)
    ;; Remove all overlays from this list that don't have an associated buffer.
    (setq magit-commit-mark--overlays
          (delq
           nil (mapcar (lambda (ov) (and (overlay-buffer ov) ov)) magit-commit-mark--overlays))))

  (let ((point-prev nil)

        ;; Precomputed flags.
        (flag-read (ash 1 magit-commit-mark--bitflag-read))
        (flag-star (ash 1 magit-commit-mark--bitflag-star))
        (flag-urgent (ash 1 magit-commit-mark--bitflag-urgent)))

    ;; Move to line start.
    (goto-char (pos-bol point-beg))

    (while (and (< (point) point-end) (null (eq (point) point-prev)))
      (let ((point-sha1-beg (point))
            (point-sha1-end nil)
            (point-star-beg nil)
            (point-star-end nil)
            (point-subject-beg nil)
            (point-subject-end (pos-eol)))

        (unless (zerop (skip-chars-forward "^[:blank:]" point-subject-end))
          (setq point-sha1-end (point))
          (goto-char point-sha1-beg)
          (let ((sha1 (buffer-substring-no-properties point-sha1-beg point-sha1-end)))
            ;; Validate: the line should start with a hex SHA1.
            (when (string-match-p "\\`[[:xdigit:]]+\\'" sha1)
              (setq sha1 (substring sha1 0 magit-commit-mark-sha1-length))
              (let* ((value (or (gethash sha1 repo-hash) 0))
                     (is-read (null (zerop (logand value flag-read))))
                     (is-star (null (zerop (logand value flag-star))))
                     (is-urgent (null (zerop (logand value flag-urgent)))))

                (when (or is-urgent is-star)
                  (goto-char point-sha1-end)
                  (skip-chars-forward "[:blank:]*|\\\\/" point-subject-end)
                  (setq point-subject-beg (point))
                  (goto-char point-sha1-beg))

                (when is-star
                  (goto-char point-sha1-end)
                  (skip-chars-forward "^*" point-subject-beg)
                  (cond
                   ((eq ?\* (char-after (point)))
                    (setq point-star-beg (1- (point)))
                    (setq point-star-end (+ (point) 2)))
                   (t
                    ;; Fallback if star marker not found.
                    (setq point-star-beg point-sha1-end)
                    (setq point-star-end point-subject-beg))))

                ;; Read status.
                (let ((ov (make-overlay point-sha1-beg point-sha1-end)))
                  (overlay-put ov 'magit-commit-mark t)
                  (overlay-put
                   ov 'face
                   (cond
                    (is-read
                     'magit-commit-mark-read-face)
                    (t
                     'magit-commit-mark-unread-face)))
                  (push ov magit-commit-mark--overlays))

                ;; Starred status.
                (when is-star
                  (let ((ov (make-overlay point-star-beg point-star-end)))
                    (overlay-put ov 'magit-commit-mark t)
                    (overlay-put ov 'face 'magit-commit-mark-star-face)
                    (push ov magit-commit-mark--overlays)))

                ;; Urgent status.
                (when is-urgent
                  (let ((ov (make-overlay point-subject-beg (1+ point-subject-end))))
                    (overlay-put ov 'magit-commit-mark t)
                    (overlay-put ov 'face 'magit-commit-mark-urgent-face)
                    (push ov magit-commit-mark--overlays)))))))

        ;; Record position for loop termination check.
        (setq point-prev point-sha1-beg))
      (forward-line 1))))

(defun magit-commit-mark--overlay-refresh (repo-hash)
  "Refresh all commit mark overlays using REPO-HASH."
  (declare (important-return-value nil))
  (save-excursion
    (magit-commit-mark--overlay-refresh-range
     repo-hash
     (save-excursion
       (goto-char (max (point-min) (window-start)))
       (pos-bol))
     (save-excursion
       (goto-char (min (point-max) (window-end)))
       (pos-eol)))))


;; ---------------------------------------------------------------------------
;; Hash Access

(defun magit-commit-mark--hash-create ()
  "Return a new empty hash table for storing SHA1 data."
  (declare (important-return-value t))
  (make-hash-table :test 'equal))

(defun magit-commit-mark--hash-ensure (repo-dir &optional no-file-read)
  "Ensure REPO-DIR has a hash table entry.

When NO-FILE-READ is non-nil, initialize with an empty hash table."
  (declare (important-return-value t))
  (let ((cell (assoc repo-dir magit-commit-mark--repo-hashes)))
    (cond
     (cell
      (cdr cell))
     ;; Initialize with empty hash table.
     (no-file-read
      (let ((repo-hash (magit-commit-mark--hash-create)))
        (push (cons repo-dir repo-hash) magit-commit-mark--repo-hashes)
        repo-hash))
     ;; Initialize from file.
     (t
      (magit-commit-mark--hashfile-read-with-dir repo-dir)))))

(defun magit-commit-mark--hash-ensure-or-error (repo-dir &optional no-file-read)
  "A wrapper for `magit-commit-mark--hash-ensure' that raises an error on failure.

See `magit-commit-mark--hash-ensure' for REPO-DIR and NO-FILE-READ arguments."
  (declare (important-return-value t))
  (let ((repo-hash (magit-commit-mark--hash-ensure repo-dir no-file-read)))
    (unless repo-hash
      (error "No internal hash table in %S" repo-dir))
    repo-hash))

(defun magit-commit-mark--hash-set (repo-dir repo-hash)
  "Set REPO-HASH for REPO-DIR in `magit-commit-mark--repo-hashes'."
  (declare (important-return-value nil))
  (let ((cell (assoc repo-dir magit-commit-mark--repo-hashes)))
    (cond
     (cell
      (setcdr cell repo-hash))
     (t
      (push (cons repo-dir repo-hash) magit-commit-mark--repo-hashes)))))


;; ---------------------------------------------------------------------------
;; Hash Disk I/O

(defun magit-commit-mark--hashfile-read-with-dir (repo-dir)
  "Load hash table from file for REPO-DIR."
  (declare (important-return-value t))
  (let ((hash-file (magit-commit-mark--make-file-name-from-repo repo-dir)))
    (let ((repo-hash
           (cond
            ((file-exists-p hash-file)
             (with-temp-buffer
               (insert-file-contents hash-file)
               (goto-char (point-min))
               (read (current-buffer))))
            (t
             (magit-commit-mark--hash-create)))))

      ;; Sanity check: should never happen with valid data files.
      (unless (hash-table-p repo-hash)
        (error "Unexpected type for internal hash table %S" (type-of repo-hash)))

      (magit-commit-mark--hash-set repo-dir repo-hash)

      repo-hash)))

(defun magit-commit-mark--hashfile-write-with-dir (repo-dir)
  "Write the hash table to file for REPO-DIR."
  (declare (important-return-value nil))
  (unless (file-directory-p magit-commit-mark-directory)
    (make-directory magit-commit-mark-directory t))
  (let ((hash-file (magit-commit-mark--make-file-name-from-repo repo-dir)))
    (let ((repo-hash (magit-commit-mark--hash-ensure repo-dir)))
      (unless (hash-table-p repo-hash)
        (setq repo-hash (magit-commit-mark--hash-create)))

      ;; Backup the file before writing for safety.
      ;; Overwriting the previous backup is acceptable.
      (when (file-exists-p hash-file)
        (rename-file hash-file (concat hash-file ".backup") t))

      (with-temp-buffer
        (prin1 repo-hash (current-buffer))
        ;; The 0 argument suppresses the "Wrote file" message.
        (write-region nil nil hash-file nil 0)))))

(defun magit-commit-mark--hashfile-write ()
  "Write the hash table to file for the current repository."
  (declare (important-return-value nil))
  (let ((repo-dir (magit-commit-mark--get-repo-dir-or-error)))
    (magit-commit-mark--hashfile-write-with-dir repo-dir)))


;; ---------------------------------------------------------------------------
;; Internal Commit Mark Set/Clear/Toggle Implementation

(defun magit-commit-mark--commit-at-point-manipulate-with-sha1 (repo-dir sha1 action bit)
  "Manipulate BIT for SHA1 in REPO-DIR using ACTION (set, clear, or toggle)."
  (declare (important-return-value nil))
  (let* ((repo-hash (magit-commit-mark--hash-ensure repo-dir))
         (flag (ash 1 bit))
         (value-real (gethash sha1 repo-hash))
         (value (or value-real 0))
         (value-next
          (pcase action
            ('set (logior value flag))
            ('clear (logand value (lognot flag)))
            ('toggle (logxor value flag))
            (_ (message "Unknown action %S" action)))))

    ;; When the result is true, the value changed, update the file.
    (when (cond
           ;; Do nothing, unknown action.
           ((null value-next)
            nil)
           ;; Do nothing, no value exists and it remains zero.
           ((and (null value-real) (zerop value-next))
            nil)
           ;; Do nothing, value is unchanged.
           ((eq value value-next)
            nil)
           ;; Value was removed.
           ((zerop value-next)
            (remhash sha1 repo-hash)
            t)
           ;; Value was added or updated.
           (t
            (puthash sha1 value-next repo-hash)
            t))

      (magit-commit-mark--overlay-refresh repo-hash)
      (magit-commit-mark--hashfile-write))))


;; ---------------------------------------------------------------------------
;; Step to Commit by Flag

(defun magit-commit-mark--step-to-bit-test-at-point (repo-hash state flag)
  "Check if FLAG is set to STATE in REPO-HASH for the commit at point."
  (declare (important-return-value t))
  (let ((sha1 (magit-commit-mark--get-sha1-at-point-or-nil)))
    (when sha1
      (let ((value (or (gethash sha1 repo-hash) 0)))
        (eq state (null (zerop (logand value flag))))))))

(defun magit-commit-mark--step-to-bit-test-at-point-strict (repo-hash state flag)
  "Check if FLAG is set to STATE in REPO-HASH for the commit at point.
This is a strict version that requires the SHA1 to be at the line start,
useful for merge commits that show branching lines."
  (declare (important-return-value t))
  (unless (eq ?\s (char-after (pos-bol)))
    (magit-commit-mark--step-to-bit-test-at-point repo-hash state flag)))

(defun magit-commit-mark--step-to-bit (dir state bit)
  "Move in direction DIR to the next commit with BIT set to STATE."
  (declare (important-return-value t))
  ;; NOTE: don't depend on the display state, access the hash table directly.
  (let* ((repo-dir (magit-commit-mark--get-repo-dir))
         (repo-hash (magit-commit-mark--hash-ensure repo-dir))
         (flag (ash 1 bit))
         (point-prev nil)
         (found nil)
         (found-point nil))

    (save-excursion
      (forward-line dir)

      (while (and (null (eq point-prev (point)))
                  (null
                   (setq found
                         (magit-commit-mark--step-to-bit-test-at-point-strict
                          repo-hash state flag))))
        (setq point-prev (point))
        (forward-line dir))

      (when found
        (setq found-point (point))))

    (cond
     (found-point
      (goto-char found-point)

      ;; Use maybe-update with a zero timer instead of `(call-interactively 'magit-show-commit)'
      ;; because it handles canceling the idle timer, avoiding an occasional glitch
      ;; where the idle timer opens a different commit than this one.
      (let ((magit-update-other-window-delay 0.0))
        (magit-log-maybe-update-revision-buffer))
      t)
     (t
      nil))))


(defun magit-commit-mark--commit-at-point-action-on-bit (action bit)
  "Perform ACTION on flag BIT for the commit at point."
  (declare (important-return-value nil))
  (pcase-let ((`(,repo-dir . ,sha1) (magit-commit-mark--get-context-vars-or-error)))
    (magit-commit-mark--commit-at-point-manipulate-with-sha1 repo-dir sha1 action bit)))

;; NOTE: it's important to move to the beginning of the line since the user may have
;; moved the cursor elsewhere, causing the SHA1 not to be detected.
(defun magit-commit-mark--commit-at-point-action-on-bit-bol (action bit)
  "Perform ACTION on flag BIT for the commit at point (at line start)."
  (declare (important-return-value nil))
  (save-excursion
    (goto-char (pos-bol))
    (magit-commit-mark--commit-at-point-action-on-bit action bit)))


;; ---------------------------------------------------------------------------
;; Report Marked Commits

(defun magit-commit-mark--report-commits-by-bit (repo-hash bit)
  "Report all commits in the region or buffer using REPO-HASH and BIT."
  (declare (important-return-value nil))
  (save-excursion
    (save-restriction
      (apply #'narrow-to-region (magit-commit-mark--region-range-or-all))
      (goto-char (point-min))
      (let ((search t)
            (flag (ash 1 bit)))
        (while search
          (let ((sha1 (magit-commit-mark--get-sha1-at-point-or-nil)))
            (when sha1
              (let ((value (gethash sha1 repo-hash)))
                (when value
                  (unless (zerop (logand value flag))
                    ;; NOTE: Consider logging to a dedicated buffer.
                    (message "%s" sha1))))))
          (unless (zerop (forward-line 1))
            (setq search nil)))))))


;; ---------------------------------------------------------------------------
;; Internal Integration Functions

(defun magit-commit-mark--show-commit-timer (buf point-beg repo-dir sha1)
  "Timer callback to mark a commit as read after the delay.
BUF is the buffer where the commit was shown.
POINT-BEG is used to check if the current line has changed.
REPO-DIR and SHA1 are forwarded to
`magit-commit-mark--commit-at-point-manipulate-with-sha1'."
  (declare (important-return-value nil))
  ;; Only proceed if buffer is still alive.
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (eq point-beg (pos-bol))
        (magit-commit-mark--commit-at-point-manipulate-with-sha1
         repo-dir sha1 'set magit-commit-mark--bitflag-read)))))

(defun magit-commit-mark--show-commit-advice (old-fn &rest args)
  "Advice function for `magit-show-commit'.

This calls OLD-FN with ARGS."
  (declare (important-return-value nil))
  ;; We only care about the SHA1, other values aren't important.
  (let ((sha1 (car args))
        ;; Demote error so it's obvious marking failed and
        ;; `old-fn' is never prevented from running.
        ;; In practice this should never happen, it's mainly to prevent this package
        ;; from breaking `magit' even in the chance of extremely unlikely situations.
        (repo-dir
         (with-demoted-errors "%S"
           (magit-commit-mark--get-repo-dir-or-error))))
    (cond
     ;; The error will have been demoted to a message, do nothing.
     ((null repo-dir)
      nil)
     ;; No delay.
     ((zerop magit-commit-mark-on-show-commit-delay)
      (with-demoted-errors "%S"
        (magit-commit-mark--commit-at-point-manipulate-with-sha1
         repo-dir sha1 'set magit-commit-mark--bitflag-read)))
     ;; Use timer for delay.
     (t
      (when magit-commit-mark--on-show-commit-global-timer
        (cancel-timer magit-commit-mark--on-show-commit-global-timer))
      (setq magit-commit-mark--on-show-commit-global-timer
            (run-with-idle-timer
             magit-commit-mark-on-show-commit-delay nil #'magit-commit-mark--show-commit-timer
             (current-buffer) (pos-bol) repo-dir sha1)))))

  ;; Call the original function.
  (apply old-fn args))

(defun magit-commit-mark--magit-log-arguments-extra (fn-orig &optional mode)
  "Add extra arguments to git log view around FN-ORIG, optionally passing MODE.
Needed so we can be sure to view the required number of SHA1 chars."
  (declare (important-return-value t))
  (pcase-let ((`(,args ,files) (funcall fn-orig mode)))
    (let ((arg (format "--abbrev=%d" magit-commit-mark-sha1-length)))
      ;; When called multiple times, the argument may already be in the list.
      (unless (member arg args)
        (setq args (append args (list arg))))
      (list args files))))


;; ---------------------------------------------------------------------------
;; Immediate Font Locking

(defun magit-commit-mark--font-lock-fontify-region (point-beg point-end)
  "Update commit mark overlays between POINT-BEG and POINT-END."
  (declare (important-return-value nil))
  (let ((repo-dir (magit-commit-mark--get-repo-dir)))
    (let ((repo-hash (magit-commit-mark--hash-ensure repo-dir)))
      (magit-commit-mark--overlay-refresh-range repo-hash point-beg point-end))))

(defun magit-commit-mark--immediate-enable ()
  "Enable immediate commit mark display."
  (declare (important-return-value nil))
  (jit-lock-register #'magit-commit-mark--font-lock-fontify-region))

(defun magit-commit-mark--immediate-disable ()
  "Disable immediate commit mark display."
  (declare (important-return-value nil))
  (jit-lock-unregister #'magit-commit-mark--font-lock-fontify-region)
  (magit-commit-mark--overlay-clear))


;; ---------------------------------------------------------------------------
;; Internal Mode Enable/Disable

(defun magit-commit-mark--enable ()
  "Enable the buffer-local minor mode."
  (declare (important-return-value nil))

  (advice-add 'magit-log-arguments :around #'magit-commit-mark--magit-log-arguments-extra)

  (when magit-commit-mark-on-show-commit
    (advice-add 'magit-show-commit :around #'magit-commit-mark--show-commit-advice))

  (when (derived-mode-p 'magit-log-mode)
    (magit-commit-mark--immediate-enable)))

(defun magit-commit-mark--disable ()
  "Disable the buffer-local minor mode."
  (declare (important-return-value nil))

  (advice-remove 'magit-log-arguments #'magit-commit-mark--magit-log-arguments-extra)

  (when magit-commit-mark-on-show-commit
    (advice-remove 'magit-show-commit #'magit-commit-mark--show-commit-advice))

  (when magit-commit-mark--on-show-commit-global-timer
    (cancel-timer magit-commit-mark--on-show-commit-global-timer))

  (when (derived-mode-p 'magit-log-mode)
    (magit-commit-mark--immediate-disable)
    (kill-local-variable 'magit-commit-mark--overlays)))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun magit-commit-mark-toggle-read ()
  "Toggle the commit at point read status."
  (declare (important-return-value nil))
  (interactive)
  (magit-commit-mark--commit-at-point-action-on-bit-bol 'toggle magit-commit-mark--bitflag-read))

;;;###autoload
(defun magit-commit-mark-toggle-star ()
  "Toggle the commit at point star status."
  (declare (important-return-value nil))
  (interactive)
  (magit-commit-mark--commit-at-point-action-on-bit-bol 'toggle magit-commit-mark--bitflag-star))

;;;###autoload
(defun magit-commit-mark-toggle-urgent ()
  "Toggle the commit at point urgent status."
  (declare (important-return-value nil))
  (interactive)
  (magit-commit-mark--commit-at-point-action-on-bit-bol 'toggle magit-commit-mark--bitflag-urgent))

;; NOTE: other stepping functions could be added,
;; for now stepping by unread seems the most useful.

;;;###autoload
(defun magit-commit-mark-next-unread ()
  "Jump to the next unread commit."
  (declare (important-return-value nil))
  (interactive)
  (let ((bit magit-commit-mark--bitflag-read))
    (cond
     ((magit-commit-mark--step-to-bit 1 nil bit)
      (when magit-commit-mark-on-skip-to-unread
        (magit-commit-mark--commit-at-point-action-on-bit-bol 'set bit))
      t)
     (t
      (message "No unread commits in view (next)")
      (ding t)
      nil))))

;;;###autoload
(defun magit-commit-mark-prev-unread ()
  "Jump to the previous unread commit."
  (declare (important-return-value nil))
  (interactive)
  (let ((bit magit-commit-mark--bitflag-read))
    (cond
     ((magit-commit-mark--step-to-bit -1 nil bit)
      (when magit-commit-mark-on-skip-to-unread
        (magit-commit-mark--commit-at-point-action-on-bit-bol 'set bit))
      t)
     (t
      (message "No unread commits in view (previous)")
      (ding t)
      nil))))

;;;###autoload
(defun magit-commit-mark-report-urgent ()
  "Report all urgent commits in the region or buffer."
  (declare (important-return-value nil))
  (interactive)
  (let* ((repo-dir (magit-commit-mark--get-repo-dir-or-error))
         (repo-hash (magit-commit-mark--hash-ensure-or-error repo-dir)))
    (magit-commit-mark--report-commits-by-bit repo-hash magit-commit-mark--bitflag-urgent)))

;;;###autoload
(defun magit-commit-mark-report-star ()
  "Report all starred commits in the region or buffer."
  (declare (important-return-value nil))
  (interactive)
  (let* ((repo-dir (magit-commit-mark--get-repo-dir-or-error))
         (repo-hash (magit-commit-mark--hash-ensure-or-error repo-dir)))
    (magit-commit-mark--report-commits-by-bit repo-hash magit-commit-mark--bitflag-star)))

;;;###autoload
(define-minor-mode magit-commit-mark-mode
  "Minor mode for marking commits as read/unread in Magit log buffers.

When enabled, commits are visually distinguished by their read status
and can be marked as read, starred, or urgent.  The state is persisted
between sessions."
  :global nil

  (cond
   (magit-commit-mark-mode
    (magit-commit-mark--enable))
   (t
    (magit-commit-mark--disable))))

(provide 'magit-commit-mark)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; magit-commit-mark.el ends here
