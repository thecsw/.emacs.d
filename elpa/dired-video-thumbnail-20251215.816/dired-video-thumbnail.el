;;; dired-video-thumbnail.el --- Display video thumbnails from dired -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Package-Version: 20251215.816
;; Package-Revision: 4735c6d81c48
;; Package-Requires: ((emacs "28.1"))
;; Keywords: multimedia, files, dired
;; URL: https://github.com/captainflasmr/dired-video-thumbnail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides image-dired style thumbnail viewing for video files.
;; It uses ffmpeg to extract thumbnails and displays them in a grid layout.
;;
;; Features:
;; - Persistent thumbnail caching (thumbnails are generated once and reused)
;; - Async thumbnail generation (Emacs remains responsive)
;; - Grid layout display similar to image-dired
;; - Click to play with customisable video player
;; - Works with marked files or all videos in directory
;;
;; Requirements:
;; - ffmpeg must be installed and in your PATH
;;
;; Usage:
;; In a Dired buffer, call `dired-video-thumbnail' to display thumbnails
;; for all video files (or marked files if any are marked).
;;
;; Keybindings in the thumbnail buffer:
;; - RET / mouse-1: Play video
;; - g: Regenerate thumbnail for video at point
;; - G: Regenerate all thumbnails
;; - d: Open Dired at video's directory
;; - q: Quit thumbnail buffer
;; - n/p: Next/previous video
;; - +/-: Increase/decrease thumbnail size

;;; Code:

(require 'dired)
(require 'image)
(require 'cl-seq)

;;; Customisation

(defgroup dired-video-thumbnail nil
  "Display video thumbnails from Dired."
  :group 'dired
  :prefix "dired-video-thumbnail-")

(defcustom dired-video-thumbnail-cache-dir
  (expand-file-name "dired-video-thumbnails" user-emacs-directory)
  "Directory for caching video thumbnails."
  :type 'directory
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-size 200
  "Width of generated thumbnails in pixels.
Height is calculated automatically to maintain aspect ratio."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-display-height 150
  "Display height of thumbnails in the buffer.
Set to nil to use actual thumbnail size."
  :type '(choice integer (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-columns 4
  "Number of thumbnail columns in the display buffer.
Only used when `dired-video-thumbnail-wrap-display' is nil."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-wrap-display t
  "Whether to wrap thumbnails to fill the buffer width.
When non-nil, thumbnails flow naturally and wrap based on window width.
When nil, a fixed number of columns is used."
  :type 'boolean
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-spacing 4
  "Spacing between thumbnails in pixels when using wrap display."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-timestamp "00:00:05"
  "Timestamp to extract thumbnail from (HH:MM:SS format).
Set to nil to let ffmpeg choose automatically."
  :type '(choice string (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-video-player "mpv"
  "Command to use for playing videos.
Set to nil to use `browse-url-xdg-open' or system default."
  :type '(choice string (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-video-extensions
  '("mp4" "mkv" "avi" "mov" "webm" "m4v" "wmv" "flv" "mpeg" "mpg" "ogv" "3gp")
  "List of video file extensions to recognise."
  :type '(repeat string)
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-ffmpeg-program "ffmpeg"
  "Path to ffmpeg executable."
  :type 'string
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-ffprobe-program "ffprobe"
  "Path to ffprobe executable."
  :type 'string
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-mark-border-width 4
  "Width of the border around marked thumbnails in pixels."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-sort-by 'name
  "Default sorting criteria for thumbnails."
  :type '(choice (const :tag "Name" name)
                 (const :tag "Date modified" date)
                 (const :tag "Size" size)
                 (const :tag "Duration" duration))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-sort-order 'ascending
  "Default sort order for thumbnails."
  :type '(choice (const :tag "Ascending" ascending)
                 (const :tag "Descending" descending))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-recursive nil
  "Whether to search for videos recursively in subdirectories.
When non-nil, `dired-video-thumbnail' will include videos from
all subdirectories."
  :type 'boolean
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-auto-recursive t
  "Whether to automatically search recursively when no videos in current directory.
When non-nil, if the current directory contains no video files but has
subdirectories, `dired-video-thumbnail' will automatically search recursively.
This is useful for video collections organised in year/category folders."
  :type 'boolean
  :group 'dired-video-thumbnail)

(defface dired-video-thumbnail-mark
  '((t :foreground "red"))
  "Face for the border around marked video thumbnails."
  :group 'dired-video-thumbnail)

;;; Internal variables

(defvar dired-video-thumbnail--processes nil
  "List of active thumbnail generation processes.")

(defvar dired-video-thumbnail--pending nil
  "List of videos pending thumbnail generation.")

(defvar dired-video-thumbnail--video-info-cache (make-hash-table :test 'equal)
  "Cache for video metadata (dimensions, duration).")

(defvar dired-video-thumbnail--current-videos nil
  "List of videos in the current thumbnail buffer.")

(defvar dired-video-thumbnail--all-videos nil
  "List of all videos before filtering (buffer-local).")

(defvar dired-video-thumbnail--source-dir nil
  "Source directory for the current thumbnail buffer.")

(defvar-local dired-video-thumbnail--dired-buffer nil
  "The Dired buffer associated with this thumbnail buffer.")

(defvar-local dired-video-thumbnail--video-at-point nil
  "Video file path at the current position.")

(defvar-local dired-video-thumbnail--sort-by nil
  "Current sort criteria for this buffer.")

(defvar-local dired-video-thumbnail--sort-order nil
  "Current sort order for this buffer.")

(defvar-local dired-video-thumbnail--filter-name nil
  "Current name filter regexp.")

(defvar-local dired-video-thumbnail--filter-duration-min nil
  "Minimum duration filter in seconds.")

(defvar-local dired-video-thumbnail--filter-duration-max nil
  "Maximum duration filter in seconds.")

(defvar-local dired-video-thumbnail--filter-size-min nil
  "Minimum size filter in bytes.")

(defvar-local dired-video-thumbnail--filter-size-max nil
  "Maximum size filter in bytes.")

(defvar-local dired-video-thumbnail--recursive nil
  "Whether current buffer is showing videos recursively.")

(defvar-local dired-video-thumbnail--local-marks nil
  "Hash table of locally marked files (for files not in dired buffer).")

;;; Utility functions

(defun dired-video-thumbnail--ensure-cache-dir ()
  "Ensure the thumbnail cache directory exists."
  (unless (file-directory-p dired-video-thumbnail-cache-dir)
    (make-directory dired-video-thumbnail-cache-dir t)))

(defun dired-video-thumbnail--video-p (file)
  "Return non-nil if FILE is a video file."
  (and (file-regular-p file)
       (member (downcase (or (file-name-extension file) ""))
               dired-video-thumbnail-video-extensions)))

(defun dired-video-thumbnail--find-videos (directory &optional recursive)
  "Find all video files in DIRECTORY.
If RECURSIVE is non-nil, search subdirectories as well."
  (if recursive
      (let ((videos nil))
        (dolist (file (directory-files-recursively
                       directory
                       (concat "\\." (regexp-opt dired-video-thumbnail-video-extensions) "\\'")
                       nil))
          (when (dired-video-thumbnail--video-p file)
            (push file videos)))
        (nreverse videos))
    (seq-filter #'dired-video-thumbnail--video-p
                (directory-files directory t nil t))))

(defun dired-video-thumbnail--file-marked-p (file)
  "Return non-nil if FILE is marked.
Checks both the associated dired buffer and local marks (for recursive mode)."
  (or
   ;; First check local marks (for files in subdirectories)
   (and dired-video-thumbnail--local-marks
        (gethash file dired-video-thumbnail--local-marks))
   ;; Then check dired buffer
   (when (and dired-video-thumbnail--dired-buffer
              (buffer-live-p dired-video-thumbnail--dired-buffer))
     (with-current-buffer dired-video-thumbnail--dired-buffer
       (save-excursion
         (goto-char (point-min))
         (when (dired-goto-file file)
           (beginning-of-line)
           (looking-at-p dired-re-mark)))))))

(defun dired-video-thumbnail--file-in-dired-p (file)
  "Return non-nil if FILE is visible in the associated dired buffer."
  (when (and dired-video-thumbnail--dired-buffer
             (buffer-live-p dired-video-thumbnail--dired-buffer))
    (with-current-buffer dired-video-thumbnail--dired-buffer
      (save-excursion
        (goto-char (point-min))
        (dired-goto-file file)))))

(defun dired-video-thumbnail--ensure-subdir-in-dired (file dired-buf source-dir)
  "Ensure the subdirectory containing FILE is inserted in dired.
DIRED-BUF is the dired buffer to insert into.
SOURCE-DIR is the root directory.
Returns non-nil if the file can now be found in dired."
  (when (and dired-buf (buffer-live-p dired-buf) source-dir)
    (let* ((file-dir (file-name-directory (expand-file-name file)))
           (source-dir-exp (expand-file-name source-dir)))
      ;; Only insert if file is in a subdirectory of source-dir
      (when (and (string-prefix-p source-dir-exp file-dir)
                 (not (string= source-dir-exp file-dir)))
        (with-current-buffer dired-buf
          ;; Check if file is already accessible
          (save-excursion
            (goto-char (point-min))
            (unless (dired-goto-file file)
              ;; Need to insert the subdirectory
              (condition-case err
                  (progn
                    (dired-insert-subdir file-dir)
                    t)
                (error 
                 (message "Could not insert subdir %s: %s" file-dir err)
                 nil)))))))))

(defun dired-video-thumbnail--mark-in-dired (file mark)
  "Set MARK on FILE in the associated dired buffer or local marks.
MARK should be ?* to mark or ?\\s (space) to unmark.
For files in subdirectories, inserts the subdirectory into dired first."
  ;; Capture buffer-local variables before switching buffers
  (let ((dired-buf dired-video-thumbnail--dired-buffer)
        (source-dir dired-video-thumbnail--source-dir))
    (if (not (and dired-buf (buffer-live-p dired-buf)))
        (message "No live dired buffer associated!")
      ;; Try to mark in dired
      (let ((found-in-dired nil))
        (with-current-buffer dired-buf
          ;; First check if file exists in dired
          (save-excursion
            (goto-char (point-min))
            (unless (dired-goto-file file)
              ;; File not found, try inserting its subdirectory
              (dired-video-thumbnail--ensure-subdir-in-dired file dired-buf source-dir)))
          ;; Now try to mark
          (save-excursion
            (goto-char (point-min))
            (when (dired-goto-file file)
              (setq found-in-dired t)
              (let ((inhibit-read-only t))
                (beginning-of-line)
                (delete-char 1)
                (insert-char mark)))))
        ;; If still not in dired, use local marks as fallback
        (unless found-in-dired
          (unless dired-video-thumbnail--local-marks
            (setq dired-video-thumbnail--local-marks (make-hash-table :test 'equal)))
          (if (eq mark ?*)
              (puthash file t dired-video-thumbnail--local-marks)
            (remhash file dired-video-thumbnail--local-marks)))))))

(defun dired-video-thumbnail-debug ()
  "Show debug info about current state."
  (interactive)
  (message "Dired buffer: %s (live: %s), Source dir: %s, Videos: %d, Current file: %s"
           dired-video-thumbnail--dired-buffer
           (and dired-video-thumbnail--dired-buffer
                (buffer-live-p dired-video-thumbnail--dired-buffer))
           dired-video-thumbnail--source-dir
           (length dired-video-thumbnail--current-videos)
           (get-text-property (point) 'dired-video-thumbnail-file)))

(defun dired-video-thumbnail--cache-path (video-file)
  "Return the cached thumbnail path for VIDEO-FILE."
  (dired-video-thumbnail--ensure-cache-dir)
  (let* ((full-path (expand-file-name video-file))
         (attrs (file-attributes full-path))
         (mtime (format-time-string "%Y%m%d%H%M%S"
                                    (file-attribute-modification-time attrs)))
         (hash (md5 (concat full-path mtime))))
    (expand-file-name (concat hash ".jpg") dired-video-thumbnail-cache-dir)))

(defun dired-video-thumbnail--cached-p (video-file)
  "Return non-nil if VIDEO-FILE has a cached thumbnail."
  (let ((cache-path (dired-video-thumbnail--cache-path video-file)))
    (and (file-exists-p cache-path)
         (> (file-attribute-size (file-attributes cache-path)) 0))))

;;; Video information

(defun dired-video-thumbnail--get-video-info (video-file)
  "Get video information for VIDEO-FILE.
Returns a plist with :width, :height, :duration, or nil on failure.
Results are cached."
  (or (gethash video-file dired-video-thumbnail--video-info-cache)
      (let ((info (dired-video-thumbnail--fetch-video-info video-file)))
        (when info
          (puthash video-file info dired-video-thumbnail--video-info-cache))
        info)))

(defun dired-video-thumbnail--fetch-video-info (video-file)
  "Fetch video information for VIDEO-FILE using ffprobe."
  (condition-case nil
      (let ((output (shell-command-to-string
                     (format "%s -v error -select_streams v:0 -show_entries stream=width,height,duration -of csv=p=0 %s"
                             dired-video-thumbnail-ffprobe-program
                             (shell-quote-argument video-file)))))
        (when (string-match "\\([0-9]+\\),\\([0-9]+\\),?\\([0-9.]*\\)" output)
          (list :width (string-to-number (match-string 1 output))
                :height (string-to-number (match-string 2 output))
                :duration (let ((dur (match-string 3 output)))
                            (if (and dur (not (string-empty-p dur)))
                                (string-to-number dur)
                              nil)))))
    (error nil)))

(defun dired-video-thumbnail--format-duration (seconds)
  "Format SECONDS as HH:MM:SS or MM:SS."
  (when seconds
    (let* ((secs (truncate seconds))
           (hours (/ secs 3600))
           (mins (/ (mod secs 3600) 60))
           (secs (mod secs 60)))
      (if (> hours 0)
          (format "%d:%02d:%02d" hours mins secs)
        (format "%d:%02d" mins secs)))))

(defun dired-video-thumbnail--format-file-size (file)
  "Return file size in MB for FILE."
  (let* ((expanded (expand-file-name file))
         (size (cond
                ;; Linux/macOS: use stat
                ((executable-find "stat")
                 (let ((size-str (string-trim
                                  (shell-command-to-string
                                   (if (eq system-type 'darwin)
                                       ;; macOS stat syntax
                                       (format "stat -f %%z %s" (shell-quote-argument expanded))
                                     ;; Linux stat syntax
                                     (format "stat -c %%s %s" (shell-quote-argument expanded)))))))
                   (string-to-number size-str)))
                ;; Windows: use PowerShell
                ((eq system-type 'windows-nt)
                 (let ((size-str (string-trim
                                  (shell-command-to-string
                                   (format "powershell -command \"(Get-Item '%s').Length\""
                                           (replace-regexp-in-string "'" "''" expanded))))))
                   (string-to-number size-str)))
                ;; Fallback: try file-attributes
                (t
                 (file-attribute-size (file-attributes expanded))))))
    (if (and size (numberp size) (> size 0))
        (format "%.1f MB" (/ (float size) (* 1024.0 1024.0)))
      "? MB")))

(defun dired-video-thumbnail--relative-name (file)
  "Return FILE name relative to the source directory."
  (if (and dired-video-thumbnail--source-dir
           (file-name-absolute-p file))
      (file-relative-name file dired-video-thumbnail--source-dir)
    (file-name-nondirectory file)))

(defun dired-video-thumbnail--header-line ()
  "Generate header line showing info for video at point."
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let* ((rel-name (dired-video-thumbnail--relative-name video))
             (info (dired-video-thumbnail--get-video-info video))
             (width (plist-get info :width))
             (height (plist-get info :height))
             (duration (plist-get info :duration))
             (size (dired-video-thumbnail--format-file-size video))
             (marked (dired-video-thumbnail--file-marked-p video)))
        (concat
         (if marked
             (propertize "* " 'face 'dired-video-thumbnail-mark)
           "  ")
         (propertize rel-name 'face 'bold)
         "  "
         (if (and width height)
             (format "%dx%d" width height)
           "")
         (if duration
             (format "  %s" (dired-video-thumbnail--format-duration duration))
           "")
         (format "  %s" size)))
    ""))

;;; Sorting and Filtering

(defun dired-video-thumbnail--get-file-size-bytes (file)
  "Return file size in bytes for FILE."
  (let* ((expanded (expand-file-name file))
         (size (cond
                ((executable-find "stat")
                 (let ((size-str (string-trim
                                  (shell-command-to-string
                                   (if (eq system-type 'darwin)
                                       (format "stat -f %%z %s" (shell-quote-argument expanded))
                                     (format "stat -c %%s %s" (shell-quote-argument expanded)))))))
                   (string-to-number size-str)))
                ((eq system-type 'windows-nt)
                 (let ((size-str (string-trim
                                  (shell-command-to-string
                                   (format "powershell -command \"(Get-Item '%s').Length\""
                                           (replace-regexp-in-string "'" "''" expanded))))))
                   (string-to-number size-str)))
                (t
                 (file-attribute-size (file-attributes expanded))))))
    (or size 0)))

(defun dired-video-thumbnail--get-file-date (file)
  "Return modification time for FILE as a float."
  (let ((attrs (file-attributes (expand-file-name file))))
    (if attrs
        (float-time (file-attribute-modification-time attrs))
      0)))

(defun dired-video-thumbnail--get-video-duration (file)
  "Return duration in seconds for VIDEO file, or 0 if unknown."
  (let ((info (dired-video-thumbnail--get-video-info file)))
    (or (plist-get info :duration) 0)))

(defun dired-video-thumbnail--sort-videos (videos)
  "Sort VIDEOS according to current sort settings."
  (let ((sort-by (or dired-video-thumbnail--sort-by dired-video-thumbnail-sort-by))
        (sort-order (or dired-video-thumbnail--sort-order dired-video-thumbnail-sort-order)))
    (let ((sorted
           (sort (copy-sequence videos)
                 (lambda (a b)
                   (let ((cmp (pcase sort-by
                                ('name (string< (downcase (file-name-nondirectory a))
                                                (downcase (file-name-nondirectory b))))
                                ('date (< (dired-video-thumbnail--get-file-date a)
                                          (dired-video-thumbnail--get-file-date b)))
                                ('size (< (dired-video-thumbnail--get-file-size-bytes a)
                                          (dired-video-thumbnail--get-file-size-bytes b)))
                                ('duration (< (dired-video-thumbnail--get-video-duration a)
                                              (dired-video-thumbnail--get-video-duration b)))
                                (_ (string< a b)))))
                     cmp)))))
      (if (eq sort-order 'descending)
          (nreverse sorted)
        sorted))))

(defun dired-video-thumbnail--filter-videos (videos)
  "Filter VIDEOS according to current filter settings."
  (let ((result videos))
    ;; Filter by name
    (when dired-video-thumbnail--filter-name
      (setq result
            (seq-filter (lambda (v)
                          (string-match-p dired-video-thumbnail--filter-name
                                          (file-name-nondirectory v)))
                        result)))
    ;; Filter by duration
    (when dired-video-thumbnail--filter-duration-min
      (setq result
            (seq-filter (lambda (v)
                          (>= (dired-video-thumbnail--get-video-duration v)
                              dired-video-thumbnail--filter-duration-min))
                        result)))
    (when dired-video-thumbnail--filter-duration-max
      (setq result
            (seq-filter (lambda (v)
                          (<= (dired-video-thumbnail--get-video-duration v)
                              dired-video-thumbnail--filter-duration-max))
                        result)))
    ;; Filter by size
    (when dired-video-thumbnail--filter-size-min
      (setq result
            (seq-filter (lambda (v)
                          (>= (dired-video-thumbnail--get-file-size-bytes v)
                              dired-video-thumbnail--filter-size-min))
                        result)))
    (when dired-video-thumbnail--filter-size-max
      (setq result
            (seq-filter (lambda (v)
                          (<= (dired-video-thumbnail--get-file-size-bytes v)
                              dired-video-thumbnail--filter-size-max))
                        result)))
    result))

(defun dired-video-thumbnail--apply-sort-and-filter ()
  "Apply current sort and filter settings and refresh display."
  (when dired-video-thumbnail--all-videos
    (let ((filtered (dired-video-thumbnail--filter-videos dired-video-thumbnail--all-videos)))
      (setq dired-video-thumbnail--current-videos
            (dired-video-thumbnail--sort-videos filtered))))
  (dired-video-thumbnail-refresh))

(defun dired-video-thumbnail--format-active-filters ()
  "Return a string describing active filters."
  (let ((filters nil))
    (when dired-video-thumbnail--filter-name
      (push (format "name:/%s/" dired-video-thumbnail--filter-name) filters))
    (when (or dired-video-thumbnail--filter-duration-min
              dired-video-thumbnail--filter-duration-max)
      (push (format "duration:%s-%s"
                    (if dired-video-thumbnail--filter-duration-min
                        (dired-video-thumbnail--format-duration dired-video-thumbnail--filter-duration-min)
                      "0")
                    (if dired-video-thumbnail--filter-duration-max
                        (dired-video-thumbnail--format-duration dired-video-thumbnail--filter-duration-max)
                      "∞"))
            filters))
    (when (or dired-video-thumbnail--filter-size-min
              dired-video-thumbnail--filter-size-max)
      (push (format "size:%s-%s"
                    (if dired-video-thumbnail--filter-size-min
                        (format "%.0fMB" (/ dired-video-thumbnail--filter-size-min (* 1024.0 1024.0)))
                      "0")
                    (if dired-video-thumbnail--filter-size-max
                        (format "%.0fMB" (/ dired-video-thumbnail--filter-size-max (* 1024.0 1024.0)))
                      "∞"))
            filters))
    (if filters
        (mapconcat #'identity (nreverse filters) " ")
      "")))

;;; Thumbnail generation

(defun dired-video-thumbnail--generate-sync (video-file)
  "Generate thumbnail for VIDEO-FILE synchronously.
Returns the thumbnail path or nil on failure."
  (let ((thumb-path (dired-video-thumbnail--cache-path video-file))
        (args (list "-i" video-file
                    "-vframes" "1"
                    "-vf" (format "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black"
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size)
                    "-y")))
    (when dired-video-thumbnail-timestamp
      (setq args (append (list "-ss" dired-video-thumbnail-timestamp) args)))
    (setq args (append args (list thumb-path)))
    (apply #'call-process dired-video-thumbnail-ffmpeg-program nil nil nil args)
    (when (and (file-exists-p thumb-path)
               (> (file-attribute-size (file-attributes thumb-path)) 0))
      thumb-path)))

(defun dired-video-thumbnail--generate-async (video-file callback)
  "Generate thumbnail for VIDEO-FILE asynchronously.
Call CALLBACK with the thumbnail path when done, or nil on failure."
  (let ((thumb-path (dired-video-thumbnail--cache-path video-file))
        (args (list "-i" video-file
                    "-vframes" "1"
                    "-vf" (format "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black"
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size)
                    "-y")))
    (when dired-video-thumbnail-timestamp
      (setq args (append (list "-ss" dired-video-thumbnail-timestamp) args)))
    (setq args (append args (list thumb-path)))
    (let ((proc (make-process
                 :name (format "video-thumb-%s" (file-name-nondirectory video-file))
                 :command (cons dired-video-thumbnail-ffmpeg-program args)
                 :sentinel (lambda (process _event)
                             (when (eq (process-status process) 'exit)
                               (setq dired-video-thumbnail--processes
                                     (delq process dired-video-thumbnail--processes))
                               (if (and (= (process-exit-status process) 0)
                                        (file-exists-p thumb-path)
                                        (> (file-attribute-size
                                            (file-attributes thumb-path)) 0))
                                   (funcall callback thumb-path)
                                 (funcall callback nil)))))))
      (push proc dired-video-thumbnail--processes)
      proc)))

;;; Display functions

(defun dired-video-thumbnail--create-placeholder ()
  "Create a placeholder image for videos without thumbnails."
  (let* ((size dired-video-thumbnail-size)
         (svg (format
               "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
                  <rect width='100%%' height='100%%' fill='#333'/>
                  <text x='50%%' y='50%%' fill='#999' font-size='14'
                        text-anchor='middle' dominant-baseline='middle'>
                    Loading...
                  </text>
                </svg>"
               size size)))
    (create-image svg 'svg t
                  :height dired-video-thumbnail-display-height)))

(defun dired-video-thumbnail--create-error-placeholder ()
  "Create a placeholder image for failed thumbnails."
  (let* ((size dired-video-thumbnail-size)
         (svg (format
               "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
                  <rect width='100%%' height='100%%' fill='#433'/>
                  <text x='50%%' y='50%%' fill='#c99' font-size='12'
                        text-anchor='middle' dominant-baseline='middle'>
                    No thumbnail
                  </text>
                </svg>"
               size size)))
    (create-image svg 'svg t
                  :height dired-video-thumbnail-display-height)))

(defun dired-video-thumbnail--create-bordered-image (image-path marked)
  "Create an image from IMAGE-PATH with optional border if MARKED."
  (if (not marked)
      (create-image image-path nil nil
                    :height dired-video-thumbnail-display-height)
    ;; Create SVG wrapper with border around the image
    (let* ((border-width dired-video-thumbnail-mark-border-width)
           (border-color (face-foreground 'dired-video-thumbnail-mark nil t))
           (size dired-video-thumbnail-size)
           (total-size (+ size (* 2 border-width)))
           ;; Read and base64 encode the image
           (image-data (with-temp-buffer
                         (insert-file-contents-literally image-path)
                         (base64-encode-string (buffer-string) t)))
           (svg (format
                 "<svg xmlns='http://www.w3.org/2000/svg'
                       xmlns:xlink='http://www.w3.org/1999/xlink'
                       width='%d' height='%d'>
                    <rect x='0' y='0' width='%d' height='%d' fill='%s'/>
                    <image x='%d' y='%d' width='%d' height='%d'
                           xlink:href='data:image/jpeg;base64,%s'/>
                  </svg>"
                 total-size total-size
                 total-size total-size border-color
                 border-width border-width size size
                 image-data)))
      (create-image svg 'svg t
                    :height dired-video-thumbnail-display-height))))

(defun dired-video-thumbnail--insert-thumbnail (video-file &optional thumb-path marked)
  "Insert thumbnail for VIDEO-FILE at point.
THUMB-PATH is the path to the thumbnail image, or nil for placeholder.
MARKED if non-nil shows the thumbnail as marked with a border."
  (let* ((rel-name (dired-video-thumbnail--relative-name video-file))
         (image (cond
                 ((and thumb-path (file-exists-p thumb-path))
                  (dired-video-thumbnail--create-bordered-image thumb-path marked))
                 ((eq thumb-path :error)
                  (dired-video-thumbnail--create-error-placeholder))
                 (t (dired-video-thumbnail--create-placeholder))))
         (start (point)))
    (insert-image image)
    (put-text-property start (point) 'dired-video-thumbnail-file video-file)
    (put-text-property start (point) 'keymap dired-video-thumbnail-item-map)
    (put-text-property start (point) 'help-echo (format "%s%s\nClick to play, m to mark"
                                                        (if marked "[MARKED] " "")
                                                        rel-name))))

(defun dired-video-thumbnail--update-thumbnail (video-file thumb-path)
  "Update the display for VIDEO-FILE with THUMB-PATH."
  (when-let ((buf (get-buffer "*Video Thumbnails*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let ((file (get-text-property (point) 'dired-video-thumbnail-file)))
              (when (string= file video-file)
                (let ((start (point))
                      (end (next-single-property-change (point)
                                                        'dired-video-thumbnail-file
                                                        nil (point-max))))
                  (delete-region start end)
                  (dired-video-thumbnail--insert-thumbnail
                   video-file
                   (or thumb-path :error)))))
            (goto-char (or (next-single-property-change (point)
                                                        'dired-video-thumbnail-file)
                           (point-max)))))))))

(defun dired-video-thumbnail--display-buffer (videos source-dir dired-buf &optional recursive)
  "Display VIDEOS in a thumbnail buffer.
SOURCE-DIR is the original dired directory.
DIRED-BUF is the associated dired buffer.
RECURSIVE indicates if videos were found recursively."
  (dired-video-thumbnail--display-buffer-internal
   videos source-dir dired-buf
   nil nil nil nil nil nil nil recursive nil))

(defun dired-video-thumbnail--display-buffer-internal (videos source-dir dired-buf
                                                              sort-by sort-order
                                                              filter-name filter-dur-min
                                                              filter-dur-max filter-size-min
                                                              filter-size-max recursive
                                                              &optional local-marks)
  "Display VIDEOS in a thumbnail buffer with sort/filter state.
SOURCE-DIR is the original dired directory.
DIRED-BUF is the associated dired buffer.
SORT-BY, SORT-ORDER, FILTER-NAME, FILTER-DUR-MIN, FILTER-DUR-MAX,
FILTER-SIZE-MIN, FILTER-SIZE-MAX preserve existing state.
RECURSIVE indicates if videos were found recursively.
LOCAL-MARKS is a hash table of locally marked files (for subdirectory files)."
  (let ((buf (get-buffer-create "*Video Thumbnails*"))
        (col 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set mode first, then set buffer-local variables after
        (dired-video-thumbnail-mode)
        ;; Restore local marks
        (setq dired-video-thumbnail--local-marks
              (or local-marks (make-hash-table :test 'equal)))
        ;; Store all videos before filtering/sorting
        (setq dired-video-thumbnail--all-videos videos)
        ;; Store recursive state
        (setq dired-video-thumbnail--recursive recursive)
        ;; Restore or initialize sort settings
        (setq dired-video-thumbnail--sort-by
              (or sort-by dired-video-thumbnail-sort-by))
        (setq dired-video-thumbnail--sort-order
              (or sort-order dired-video-thumbnail-sort-order))
        ;; Restore filter settings
        (setq dired-video-thumbnail--filter-name filter-name)
        (setq dired-video-thumbnail--filter-duration-min filter-dur-min)
        (setq dired-video-thumbnail--filter-duration-max filter-dur-max)
        (setq dired-video-thumbnail--filter-size-min filter-size-min)
        (setq dired-video-thumbnail--filter-size-max filter-size-max)
        ;; Apply filter and sort
        (let ((filtered (dired-video-thumbnail--filter-videos videos)))
          (setq dired-video-thumbnail--current-videos
                (dired-video-thumbnail--sort-videos filtered)))
        (setq dired-video-thumbnail--source-dir source-dir)
        (setq dired-video-thumbnail--dired-buffer dired-buf)
        ;; Header with sort/filter info
        (let* ((recursive-info (if dired-video-thumbnail--recursive " [recursive]" ""))
               (sort-info (format "[%s %s]"
                                  dired-video-thumbnail--sort-by
                                  (if (eq dired-video-thumbnail--sort-order 'ascending) "↑" "↓")))
               (filter-info (dired-video-thumbnail--format-active-filters))
               (showing (length dired-video-thumbnail--current-videos))
               (total (length dired-video-thumbnail--all-videos)))
          (insert (propertize (format "Video Thumbnails: %s%s  %s%s\n"
                                      (abbreviate-file-name source-dir)
                                      recursive-info
                                      sort-info
                                      (if (string-empty-p filter-info)
                                          ""
                                        (format "  %s" filter-info)))
                              'face 'header-line))
          (insert (propertize (format "Showing %d of %d videos | s: sort | /: filter | R: toggle recursive | w: toggle wrap | q: quit\n\n"
                                      showing total)
                              'face 'shadow)))
        ;; Insert thumbnails
        (if dired-video-thumbnail-wrap-display
            ;; Wrap display mode - just insert thumbnails with spaces, let them wrap
            (progn
              (dolist (video dired-video-thumbnail--current-videos)
                (let* ((cached (dired-video-thumbnail--cached-p video))
                       (marked (dired-video-thumbnail--file-marked-p video)))
                  (dired-video-thumbnail--insert-thumbnail
                   video
                   (when cached (dired-video-thumbnail--cache-path video))
                   marked))
                ;; Use a space between thumbnails
                (insert " "))
              ;; Enable word-wrap and visual-line-mode for proper wrapping
              (setq-local word-wrap t)
              (setq-local truncate-lines nil))
          ;; Fixed column mode
          (dolist (video dired-video-thumbnail--current-videos)
            (let* ((cached (dired-video-thumbnail--cached-p video))
                   (marked (dired-video-thumbnail--file-marked-p video)))
              (dired-video-thumbnail--insert-thumbnail
               video
               (when cached (dired-video-thumbnail--cache-path video))
               marked))
            (setq col (1+ col))
            (if (>= col dired-video-thumbnail-columns)
                (progn
                  (insert "\n")
                  (setq col 0))
              (insert " "))))
        (goto-char (point-min))
        ;; Move to first thumbnail
        (dired-video-thumbnail--snap-to-thumbnail)))
    (pop-to-buffer buf)
    buf))

;;; Async generation queue

(defun dired-video-thumbnail--process-queue ()
  "Process the next video in the generation queue."
  (when (and dired-video-thumbnail--pending
             (< (length dired-video-thumbnail--processes) 4))
    (let ((video (pop dired-video-thumbnail--pending)))
      (unless (dired-video-thumbnail--cached-p video)
        (message "Generating thumbnail for %s... (%d remaining)"
                 (file-name-nondirectory video)
                 (length dired-video-thumbnail--pending))
        (dired-video-thumbnail--generate-async
         video
         (lambda (thumb-path)
           (dired-video-thumbnail--update-thumbnail
            video
            (if thumb-path thumb-path :error))
           (dired-video-thumbnail--process-queue))))
      (dired-video-thumbnail--process-queue))))

(defun dired-video-thumbnail--generate-missing (videos)
  "Queue generation of missing thumbnails for VIDEOS."
  (let ((missing (seq-filter (lambda (v)
                               (not (dired-video-thumbnail--cached-p v)))
                             videos)))
    (when missing
      (setq dired-video-thumbnail--pending
            (append dired-video-thumbnail--pending missing))
      (dired-video-thumbnail--process-queue))))

;;; Interactive commands

(defun dired-video-thumbnail--has-subdirectories-p (directory)
  "Return non-nil if DIRECTORY has subdirectories."
  (let ((found nil))
    (dolist (file (directory-files directory t "^[^.]" t))
      (when (and (file-directory-p file)
                 (not (member (file-name-nondirectory file) '("." ".."))))
        (setq found t)))
    found))

;;;###autoload
(defun dired-video-thumbnail (&optional recursive)
  "Display thumbnails for video files in current dired buffer.
If files are marked, show thumbnails for marked videos only.
Otherwise, show thumbnails for all videos in the directory.
With prefix argument RECURSIVE, include videos from subdirectories.

When `dired-video-thumbnail-auto-recursive' is non-nil and the current
directory has no video files but has subdirectories, recursive mode
is automatically enabled."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let* ((dired-buf (current-buffer))
         (source-dir default-directory)
         (has-marks (save-excursion
                      (goto-char (point-min))
                      (re-search-forward dired-re-mark nil t)))
         ;; Check for videos in current directory first (non-recursive)
         (local-videos (unless has-marks
                         (dired-video-thumbnail--find-videos source-dir nil)))
         ;; Determine if we should go recursive
         (recursive-p (or recursive
                          dired-video-thumbnail-recursive
                          ;; Auto-recursive: no local videos but has subdirs
                          (and dired-video-thumbnail-auto-recursive
                               (not has-marks)
                               (null local-videos)
                               (dired-video-thumbnail--has-subdirectories-p source-dir))))
         ;; Get the final list of videos
         (videos (cond
                  (has-marks
                   (dired-get-marked-files nil nil #'dired-video-thumbnail--video-p))
                  ((and recursive-p (null local-videos))
                   ;; Need to search recursively
                   (dired-video-thumbnail--find-videos source-dir t))
                  (recursive-p
                   ;; Explicitly requested recursive
                   (dired-video-thumbnail--find-videos source-dir t))
                  (t local-videos))))
    (unless videos
      (user-error "No video files found%s"
                  (if recursive-p " (searched recursively)" "")))
    (when (and dired-video-thumbnail-auto-recursive
               (not recursive)
               (not dired-video-thumbnail-recursive)
               (null local-videos)
               recursive-p)
      (message "No videos in current directory, searching recursively..."))
    (message "Found %d video files%s"
             (length videos)
             (if recursive-p " (recursive)" ""))
    (dired-video-thumbnail--display-buffer videos source-dir dired-buf recursive-p)
    (dired-video-thumbnail--generate-missing videos)))

;;;###autoload
(defun dired-video-thumbnail-recursive ()
  "Display thumbnails for video files recursively from current directory."
  (interactive)
  (dired-video-thumbnail t))

(defun dired-video-thumbnail-play ()
  "Play the video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (cond
       ;; Custom player specified
       (dired-video-thumbnail-video-player
        (start-process "video-player" nil
                       dired-video-thumbnail-video-player video))
       ;; Windows: use start command via shell
       ((eq system-type 'windows-nt)
        (shell-command (format "start \"\" \"%s\"" (expand-file-name video))))
       ;; macOS: use open
       ((eq system-type 'darwin)
        (start-process "video-player" nil "open" video))
       ;; Linux/other: use xdg-open
       (t
        (start-process "video-player" nil "xdg-open" video)))
    (user-error "No video at point")))

(defun dired-video-thumbnail-regenerate ()
  "Regenerate the thumbnail for video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((cache-path (dired-video-thumbnail--cache-path video)))
        (when (file-exists-p cache-path)
          (delete-file cache-path))
        (message "Regenerating thumbnail for %s..." (file-name-nondirectory video))
        (dired-video-thumbnail--generate-async
         video
         (lambda (thumb-path)
           (dired-video-thumbnail--update-thumbnail video thumb-path)
           (message "Thumbnail regenerated"))))
    (user-error "No video at point")))

(defun dired-video-thumbnail-regenerate-all ()
  "Regenerate all thumbnails in the buffer."
  (interactive)
  (when dired-video-thumbnail--current-videos
    (dolist (video dired-video-thumbnail--current-videos)
      (let ((cache-path (dired-video-thumbnail--cache-path video)))
        (when (file-exists-p cache-path)
          (delete-file cache-path))))
    (dired-video-thumbnail--generate-missing dired-video-thumbnail--current-videos)))

(defun dired-video-thumbnail-next ()
  "Move to the next video thumbnail."
  (interactive)
  (let ((pos (next-single-property-change (point) 'dired-video-thumbnail-file)))
    (when pos
      (goto-char pos)
      (unless (get-text-property (point) 'dired-video-thumbnail-file)
        (goto-char (or (next-single-property-change (point)
                                                    'dired-video-thumbnail-file)
                       (point)))))))

(defun dired-video-thumbnail-previous ()
  "Move to the previous video thumbnail."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'dired-video-thumbnail-file)))
    (when pos
      (goto-char pos)
      (let ((start (previous-single-property-change (point)
                                                    'dired-video-thumbnail-file)))
        (when start
          (goto-char start))))))

(defun dired-video-thumbnail-increase-size ()
  "Increase thumbnail display size."
  (interactive)
  (setq dired-video-thumbnail-display-height
        (+ (or dired-video-thumbnail-display-height 150) 25))
  (dired-video-thumbnail-refresh))

(defun dired-video-thumbnail-decrease-size ()
  "Decrease thumbnail display size."
  (interactive)
  (setq dired-video-thumbnail-display-height
        (max 50 (- (or dired-video-thumbnail-display-height 150) 25)))
  (dired-video-thumbnail-refresh))

(defun dired-video-thumbnail-refresh ()
  "Refresh the thumbnail display."
  (interactive)
  (when (and dired-video-thumbnail--source-dir
             dired-video-thumbnail--dired-buffer)
    (let ((current-file (get-text-property (point) 'dired-video-thumbnail-file))
          ;; Preserve current sort/filter settings
          (sort-by dired-video-thumbnail--sort-by)
          (sort-order dired-video-thumbnail--sort-order)
          (filter-name dired-video-thumbnail--filter-name)
          (filter-dur-min dired-video-thumbnail--filter-duration-min)
          (filter-dur-max dired-video-thumbnail--filter-duration-max)
          (filter-size-min dired-video-thumbnail--filter-size-min)
          (filter-size-max dired-video-thumbnail--filter-size-max)
          (all-videos dired-video-thumbnail--all-videos)
          (recursive dired-video-thumbnail--recursive)
          ;; Preserve local marks
          (local-marks dired-video-thumbnail--local-marks))
      (dired-video-thumbnail--display-buffer-internal
       all-videos
       dired-video-thumbnail--source-dir
       dired-video-thumbnail--dired-buffer
       sort-by sort-order
       filter-name filter-dur-min filter-dur-max filter-size-min filter-size-max
       recursive
       local-marks)
      ;; Restore position to the same file
      (when current-file
        (dired-video-thumbnail--goto-file current-file)))))

(defun dired-video-thumbnail--goto-file (file)
  "Move point to the thumbnail for FILE."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal file (get-text-property (point) 'dired-video-thumbnail-file))
          (setq found t)
        (goto-char (or (next-single-property-change (point) 'dired-video-thumbnail-file)
                       (point-max)))))
    found))

(defun dired-video-thumbnail-clear-cache ()
  "Clear all cached thumbnails."
  (interactive)
  (when (yes-or-no-p "Clear all cached video thumbnails? ")
    (when (file-directory-p dired-video-thumbnail-cache-dir)
      (delete-directory dired-video-thumbnail-cache-dir t))
    (message "Thumbnail cache cleared")))

(defun dired-video-thumbnail-toggle-recursive ()
  "Toggle recursive display and reload videos."
  (interactive)
  (let ((new-recursive (not dired-video-thumbnail--recursive))
        (source-dir dired-video-thumbnail--source-dir)
        (dired-buf dired-video-thumbnail--dired-buffer)
        (sort-by dired-video-thumbnail--sort-by)
        (sort-order dired-video-thumbnail--sort-order)
        (filter-name dired-video-thumbnail--filter-name)
        (filter-dur-min dired-video-thumbnail--filter-duration-min)
        (filter-dur-max dired-video-thumbnail--filter-duration-max)
        (filter-size-min dired-video-thumbnail--filter-size-min)
        (filter-size-max dired-video-thumbnail--filter-size-max)
        (local-marks dired-video-thumbnail--local-marks))
    (message "Searching for videos%s..."
             (if new-recursive " recursively" ""))
    (let ((videos (dired-video-thumbnail--find-videos source-dir new-recursive)))
      (if videos
          (progn
            (dired-video-thumbnail--display-buffer-internal
             videos source-dir dired-buf
             sort-by sort-order
             filter-name filter-dur-min filter-dur-max filter-size-min filter-size-max
             new-recursive local-marks)
            (dired-video-thumbnail--generate-missing videos)
            (message "Found %d video files%s"
                     (length videos)
                     (if new-recursive " (recursive)" "")))
        (message "No video files found%s"
                 (if new-recursive " recursively" ""))))))

(defun dired-video-thumbnail-toggle-wrap ()
  "Toggle between wrap display and fixed column display."
  (interactive)
  (setq dired-video-thumbnail-wrap-display
        (not dired-video-thumbnail-wrap-display))
  (dired-video-thumbnail-refresh)
  (message "Wrap display: %s" (if dired-video-thumbnail-wrap-display "ON" "OFF")))

;;; Marking commands

(defun dired-video-thumbnail--count-marked ()
  "Count marked files in the associated Dired buffer."
  (let ((count 0))
    (dolist (video dired-video-thumbnail--current-videos)
      (when (dired-video-thumbnail--file-marked-p video)
        (setq count (1+ count))))
    count))

(defun dired-video-thumbnail-mark ()
  "Mark the video at point in both thumbnail and Dired buffers."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((count nil))
        (dired-video-thumbnail--mark-in-dired video ?*)
        (dired-video-thumbnail-refresh)
        (setq count (dired-video-thumbnail--count-marked))
        (dired-video-thumbnail-next)
        (message "Marked: %s (%d total)"
                 (file-name-nondirectory video)
                 count))
    (user-error "No video at point")))

(defun dired-video-thumbnail-unmark ()
  "Unmark the video at point in both thumbnail and Dired buffers."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((count nil))
        (dired-video-thumbnail--mark-in-dired video ?\s)
        (dired-video-thumbnail-refresh)
        (setq count (dired-video-thumbnail--count-marked))
        (dired-video-thumbnail-next)
        (message "Unmarked: %s (%d marked)"
                 (file-name-nondirectory video)
                 count))
    (user-error "No video at point")))

(defun dired-video-thumbnail-toggle-mark ()
  "Toggle mark on the video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (if (dired-video-thumbnail--file-marked-p video)
          (dired-video-thumbnail-unmark)
        (dired-video-thumbnail-mark))
    (user-error "No video at point")))

(defun dired-video-thumbnail-unmark-all ()
  "Unmark all videos in both thumbnail and Dired buffers."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (dired-video-thumbnail--mark-in-dired video ? ))
  ;; Clear local marks
  (when dired-video-thumbnail--local-marks
    (clrhash dired-video-thumbnail--local-marks))
  (dired-video-thumbnail-refresh)
  (message "All marks removed"))

(defun dired-video-thumbnail-mark-all ()
  "Mark all videos in both thumbnail and Dired buffers."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (dired-video-thumbnail--mark-in-dired video ?*))
  (dired-video-thumbnail-refresh)
  (message "Marked all %d videos" (length dired-video-thumbnail--current-videos)))

(defun dired-video-thumbnail-toggle-all-marks ()
  "Toggle mark on all videos."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (if (dired-video-thumbnail--file-marked-p video)
        (dired-video-thumbnail--mark-in-dired video ? )
      (dired-video-thumbnail--mark-in-dired video ?*)))
  (dired-video-thumbnail-refresh)
  (message "%d videos now marked" (dired-video-thumbnail--count-marked)))

(defun dired-video-thumbnail-goto-dired ()
  "Switch to the associated Dired buffer."
  (interactive)
  (if (and dired-video-thumbnail--dired-buffer
           (buffer-live-p dired-video-thumbnail--dired-buffer))
      (pop-to-buffer dired-video-thumbnail--dired-buffer)
    (when dired-video-thumbnail--source-dir
      (dired dired-video-thumbnail--source-dir))))

(defun dired-video-thumbnail-get-marked ()
  "Return list of marked videos, or video at point if none marked."
  (let ((marked (seq-filter #'dired-video-thumbnail--file-marked-p
                            dired-video-thumbnail--current-videos)))
    (or marked
        (when-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
          (list video)))))

(defun dired-video-thumbnail-delete-marked ()
  "Delete marked videos (or video at point if none marked)."
  (interactive)
  (let ((files (dired-video-thumbnail-get-marked)))
    (unless files
      (user-error "No videos to delete"))
    (when (yes-or-no-p (format "Delete %d video(s)? " (length files)))
      (dolist (file files)
        (delete-file file t)
        (setq dired-video-thumbnail--current-videos
              (delete file dired-video-thumbnail--current-videos)))
      ;; Refresh dired buffer
      (when (and dired-video-thumbnail--dired-buffer
                 (buffer-live-p dired-video-thumbnail--dired-buffer))
        (with-current-buffer dired-video-thumbnail--dired-buffer
          (revert-buffer)))
      (dired-video-thumbnail-refresh)
      (message "Deleted %d video(s)" (length files)))))

(defun dired-video-thumbnail-delete ()
  "Delete the video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (when (yes-or-no-p (format "Delete %s? " (file-name-nondirectory video)))
        ;; Find the next video to move to after deletion
        (let ((index (cl-position video dired-video-thumbnail--current-videos :test #'equal)))
          (delete-file video t)
          (setq dired-video-thumbnail--current-videos
                (delete video dired-video-thumbnail--current-videos))
          ;; Refresh dired buffer
          (when (and dired-video-thumbnail--dired-buffer
                     (buffer-live-p dired-video-thumbnail--dired-buffer))
            (with-current-buffer dired-video-thumbnail--dired-buffer
              (revert-buffer)))
          (dired-video-thumbnail-refresh)
          ;; Move to the same index position (or last if we deleted the last one)
          (when dired-video-thumbnail--current-videos
            (let ((target-index (min index (1- (length dired-video-thumbnail--current-videos)))))
              (dired-video-thumbnail--goto-nth target-index)))
          (message "Deleted %s" (file-name-nondirectory video))))
    (user-error "No video at point")))

(defun dired-video-thumbnail--goto-nth (n)
  "Move point to the Nth thumbnail (0-indexed)."
  (goto-char (point-min))
  (let ((count 0))
    (while (and (< count n)
                (not (eobp)))
      (when (dired-video-thumbnail--at-thumbnail-p)
        (setq count (1+ count)))
      (goto-char (or (next-single-property-change (point) 'dired-video-thumbnail-file)
                     (point-max))))
    ;; Make sure we're on a thumbnail
    (unless (dired-video-thumbnail--at-thumbnail-p)
      (dired-video-thumbnail--snap-to-thumbnail))))

;;; Sorting commands

(defun dired-video-thumbnail-sort ()
  "Interactively choose sort criteria."
  (interactive)
  (let ((choice (completing-read "Sort by: "
                                 '("name" "date" "size" "duration")
                                 nil t)))
    (setq dired-video-thumbnail--sort-by (intern choice))
    (dired-video-thumbnail--apply-sort-and-filter)
    (message "Sorted by %s %s"
             choice
             (if (eq dired-video-thumbnail--sort-order 'ascending) "↑" "↓"))))

(defun dired-video-thumbnail-sort-reverse ()
  "Reverse the current sort order."
  (interactive)
  (setq dired-video-thumbnail--sort-order
        (if (eq dired-video-thumbnail--sort-order 'ascending)
            'descending
          'ascending))
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "Sort order: %s"
           (if (eq dired-video-thumbnail--sort-order 'ascending)
               "ascending ↑"
             "descending ↓")))

(defun dired-video-thumbnail-sort-by-name ()
  "Sort thumbnails by filename."
  (interactive)
  (setq dired-video-thumbnail--sort-by 'name)
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "Sorted by name"))

(defun dired-video-thumbnail-sort-by-date ()
  "Sort thumbnails by modification date."
  (interactive)
  (setq dired-video-thumbnail--sort-by 'date)
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "Sorted by date"))

(defun dired-video-thumbnail-sort-by-size ()
  "Sort thumbnails by file size."
  (interactive)
  (setq dired-video-thumbnail--sort-by 'size)
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "Sorted by size"))

(defun dired-video-thumbnail-sort-by-duration ()
  "Sort thumbnails by video duration."
  (interactive)
  (setq dired-video-thumbnail--sort-by 'duration)
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "Sorted by duration"))

;;; Filtering commands

(defun dired-video-thumbnail-filter-by-name ()
  "Filter videos by filename regexp."
  (interactive)
  (let ((regexp (read-regexp "Filter by name (regexp): ")))
    (if (string-empty-p regexp)
        (setq dired-video-thumbnail--filter-name nil)
      (setq dired-video-thumbnail--filter-name regexp))
    (dired-video-thumbnail--apply-sort-and-filter)
    (message "Name filter: %s" (or dired-video-thumbnail--filter-name "none"))))

(defun dired-video-thumbnail-filter-by-duration ()
  "Filter videos by duration range."
  (interactive)
  (let* ((min-str (read-string "Minimum duration (e.g., 1:30 or 90 for seconds, empty for none): "))
         (max-str (read-string "Maximum duration (e.g., 5:00 or 300 for seconds, empty for none): "))
         (min-secs (dired-video-thumbnail--parse-duration min-str))
         (max-secs (dired-video-thumbnail--parse-duration max-str)))
    (setq dired-video-thumbnail--filter-duration-min min-secs)
    (setq dired-video-thumbnail--filter-duration-max max-secs)
    (dired-video-thumbnail--apply-sort-and-filter)
    (message "Duration filter: %s to %s"
             (if min-secs (dired-video-thumbnail--format-duration min-secs) "0")
             (if max-secs (dired-video-thumbnail--format-duration max-secs) "∞"))))

(defun dired-video-thumbnail--parse-duration (str)
  "Parse duration STR into seconds.
Accepts formats like: 90, 1:30, 1:30:00"
  (when (and str (not (string-empty-p str)))
    (let ((parts (mapcar #'string-to-number (split-string str ":"))))
      (pcase (length parts)
        (1 (car parts))  ; seconds only
        (2 (+ (* 60 (car parts)) (cadr parts)))  ; MM:SS
        (3 (+ (* 3600 (car parts)) (* 60 (cadr parts)) (caddr parts)))  ; HH:MM:SS
        (_ nil)))))

(defun dired-video-thumbnail-filter-by-size ()
  "Filter videos by file size range (in MB)."
  (interactive)
  (let* ((min-str (read-string "Minimum size in MB (empty for none): "))
         (max-str (read-string "Maximum size in MB (empty for none): "))
         (min-mb (and (not (string-empty-p min-str)) (string-to-number min-str)))
         (max-mb (and (not (string-empty-p max-str)) (string-to-number max-str))))
    (setq dired-video-thumbnail--filter-size-min
          (and min-mb (* min-mb 1024 1024)))
    (setq dired-video-thumbnail--filter-size-max
          (and max-mb (* max-mb 1024 1024)))
    (dired-video-thumbnail--apply-sort-and-filter)
    (message "Size filter: %s to %s"
             (if min-mb (format "%.0f MB" min-mb) "0")
             (if max-mb (format "%.0f MB" max-mb) "∞"))))

(defun dired-video-thumbnail-filter-clear ()
  "Clear all filters."
  (interactive)
  (setq dired-video-thumbnail--filter-name nil)
  (setq dired-video-thumbnail--filter-duration-min nil)
  (setq dired-video-thumbnail--filter-duration-max nil)
  (setq dired-video-thumbnail--filter-size-min nil)
  (setq dired-video-thumbnail--filter-size-max nil)
  (dired-video-thumbnail--apply-sort-and-filter)
  (message "All filters cleared"))

(defun dired-video-thumbnail-help ()
  "Show help for video thumbnail commands."
  (interactive)
  (with-help-window "*Video Thumbnail Help*"
    (princ "Video Thumbnail Mode Commands:\n\n")
    (princ "Navigation:\n")
    (princ "  RET, SPC, o  Play video at point\n")
    (princ "  n, →         Next thumbnail\n")
    (princ "  p, ←         Previous thumbnail\n")
    (princ "  ↑, ↓         Previous/next row\n\n")
    (princ "Marking:\n")
    (princ "  m            Mark video\n")
    (princ "  u            Unmark video\n")
    (princ "  U            Unmark all\n")
    (princ "  M            Mark all\n")
    (princ "  t            Toggle all marks\n\n")
    (princ "File Operations:\n")
    (princ "  D            Delete video at point\n")
    (princ "  x            Delete marked videos\n")
    (princ "  d            Go to Dired buffer\n\n")
    (princ "Display:\n")
    (princ "  +/-          Increase/decrease size\n")
    (princ "  r            Refresh display\n")
    (princ "  w            Toggle wrap mode\n")
    (princ "  R            Toggle recursive\n\n")
    (princ "Sorting (s prefix):\n")
    (princ "  sn           Sort by name\n")
    (princ "  sd           Sort by date\n")
    (princ "  ss           Sort by size\n")
    (princ "  sD           Sort by duration\n")
    (princ "  sr           Reverse sort order\n\n")
    (princ "Filtering (/ prefix):\n")
    (princ "  /n           Filter by name\n")
    (princ "  /d           Filter by duration\n")
    (princ "  /s           Filter by size\n")
    (princ "  /c           Clear filters\n\n")
    (princ "Other:\n")
    (princ "  g            Regenerate thumbnail\n")
    (princ "  G            Regenerate all\n")
    (princ "  q            Quit window\n")
    (princ "  h, ?         This help\n")))

(defun dired-video-thumbnail-quit-and-kill ()
  "Quit window and kill the thumbnail buffer."
  (interactive)
  (dired-video-thumbnail--cleanup)
  (kill-buffer-and-window))

(defun dired-video-thumbnail-filter ()
  "Interactively choose filter type."
  (interactive)
  (let ((choice (completing-read "Filter by: "
                                 '("name" "duration" "size" "clear all")
                                 nil t)))
    (pcase choice
      ("name" (dired-video-thumbnail-filter-by-name))
      ("duration" (dired-video-thumbnail-filter-by-duration))
      ("size" (dired-video-thumbnail-filter-by-size))
      ("clear all" (dired-video-thumbnail-filter-clear)))))

;;; Keymaps

(defvar dired-video-thumbnail-sort-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'dired-video-thumbnail-sort-by-name)
    (define-key map (kbd "d") #'dired-video-thumbnail-sort-by-date)
    (define-key map (kbd "s") #'dired-video-thumbnail-sort-by-size)
    (define-key map (kbd "D") #'dired-video-thumbnail-sort-by-duration)
    (define-key map (kbd "r") #'dired-video-thumbnail-sort-reverse)
    map)
  "Keymap for sorting commands.")

(defvar dired-video-thumbnail-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'dired-video-thumbnail-filter-by-name)
    (define-key map (kbd "d") #'dired-video-thumbnail-filter-by-duration)
    (define-key map (kbd "s") #'dired-video-thumbnail-filter-by-size)
    (define-key map (kbd "/") #'dired-video-thumbnail-filter-clear)
    (define-key map (kbd "c") #'dired-video-thumbnail-filter-clear)
    map)
  "Keymap for filtering commands.")

(defvar dired-video-thumbnail-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'dired-video-thumbnail-play)
    (define-key map [mouse-3] #'dired-video-thumbnail-toggle-mark)
    (define-key map [return] #'dired-video-thumbnail-play)
    map)
  "Keymap for individual thumbnail items.")

(defvar dired-video-thumbnail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dired-video-thumbnail-play)
    (define-key map (kbd "SPC") #'dired-video-thumbnail-play)
    (define-key map (kbd "o") #'dired-video-thumbnail-play)
    (define-key map (kbd "g") #'dired-video-thumbnail-regenerate)
    (define-key map (kbd "G") #'dired-video-thumbnail-regenerate-all)
    (define-key map (kbd "d") #'dired-video-thumbnail-goto-dired)
    (define-key map (kbd "D") #'dired-video-thumbnail-delete)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'dired-video-thumbnail-quit-and-kill)
    (define-key map (kbd "?") #'dired-video-thumbnail-help)
    (define-key map (kbd "h") #'dired-video-thumbnail-help)
    (define-key map (kbd "n") #'dired-video-thumbnail-next)
    (define-key map (kbd "p") #'dired-video-thumbnail-previous)
    (define-key map (kbd "SPC") #'dired-video-thumbnail-next)
    (define-key map (kbd "+") #'dired-video-thumbnail-increase-size)
    (define-key map (kbd "-") #'dired-video-thumbnail-decrease-size)
    (define-key map (kbd "r") #'dired-video-thumbnail-refresh)
    ;; Marking commands
    (define-key map (kbd "m") #'dired-video-thumbnail-mark)
    (define-key map (kbd "u") #'dired-video-thumbnail-unmark)
    (define-key map (kbd "U") #'dired-video-thumbnail-unmark-all)
    (define-key map (kbd "M") #'dired-video-thumbnail-mark-all)
    (define-key map (kbd "x") #'dired-video-thumbnail-delete-marked)
    (define-key map (kbd "t") #'dired-video-thumbnail-toggle-all-marks)
    ;; Navigation that skips gaps
    (define-key map (kbd "C-f") #'dired-video-thumbnail-forward)
    (define-key map (kbd "C-b") #'dired-video-thumbnail-backward)
    (define-key map (kbd "<right>") #'dired-video-thumbnail-forward)
    (define-key map (kbd "<left>") #'dired-video-thumbnail-backward)
    (define-key map (kbd "<up>") #'dired-video-thumbnail-previous-row)
    (define-key map (kbd "<down>") #'dired-video-thumbnail-next-row)
    ;; Sorting commands (s as prefix)
    (define-key map (kbd "s") dired-video-thumbnail-sort-map)
    (define-key map (kbd "S") #'dired-video-thumbnail-sort)
    ;; Filtering commands (/ as prefix)
    (define-key map (kbd "/") dired-video-thumbnail-filter-map)
    (define-key map (kbd "\\") #'dired-video-thumbnail-filter)
    ;; Recursive toggle
    (define-key map (kbd "R") #'dired-video-thumbnail-toggle-recursive)
    ;; Wrap display toggle
    (define-key map (kbd "w") #'dired-video-thumbnail-toggle-wrap)
    map)
  "Keymap for `dired-video-thumbnail-mode'.")

;;; Navigation

(defun dired-video-thumbnail-forward ()
  "Move forward to the next thumbnail."
  (interactive)
  (dired-video-thumbnail-next))

(defun dired-video-thumbnail-backward ()
  "Move backward to the previous thumbnail."
  (interactive)
  (dired-video-thumbnail-previous))

(defun dired-video-thumbnail-next-row ()
  "Move down to the next row of thumbnails."
  (interactive)
  (let ((col (dired-video-thumbnail--current-column)))
    (dotimes (_ dired-video-thumbnail-columns)
      (dired-video-thumbnail-next))
    ;; Try to maintain column position
    (let ((target-col col)
          (current-col (dired-video-thumbnail--current-column)))
      (while (and (> current-col target-col)
                  (dired-video-thumbnail--at-thumbnail-p))
        (dired-video-thumbnail-previous)
        (setq current-col (dired-video-thumbnail--current-column))))))

(defun dired-video-thumbnail-previous-row ()
  "Move up to the previous row of thumbnails."
  (interactive)
  (let ((col (dired-video-thumbnail--current-column)))
    (dotimes (_ dired-video-thumbnail-columns)
      (dired-video-thumbnail-previous))
    ;; Try to maintain column position
    (let ((target-col col)
          (current-col (dired-video-thumbnail--current-column)))
      (while (and (< current-col target-col)
                  (dired-video-thumbnail--at-thumbnail-p))
        (dired-video-thumbnail-next)
        (setq current-col (dired-video-thumbnail--current-column))))))

(defun dired-video-thumbnail--current-column ()
  "Return the column index (0-based) of the current thumbnail."
  (let ((index 0)
        (pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) pos)
                  (dired-video-thumbnail--at-thumbnail-p))
        (setq index (1+ index))
        (goto-char (or (next-single-property-change (point) 'dired-video-thumbnail-file)
                       (point-max)))))
    (mod (1- index) dired-video-thumbnail-columns)))

(defun dired-video-thumbnail--at-thumbnail-p ()
  "Return non-nil if point is at a thumbnail."
  (get-text-property (point) 'dired-video-thumbnail-file))

(defun dired-video-thumbnail--snap-to-thumbnail ()
  "Snap point to the nearest thumbnail if not already on one."
  (unless (dired-video-thumbnail--at-thumbnail-p)
    (let ((next (next-single-property-change (point) 'dired-video-thumbnail-file))
          (prev (previous-single-property-change (point) 'dired-video-thumbnail-file)))
      (cond
       ;; If we have both, go to the closer one
       ((and next prev)
        (if (< (- next (point)) (- (point) prev))
            (goto-char next)
          (goto-char prev)
          ;; Make sure we're on the thumbnail, not just after it
          (unless (dired-video-thumbnail--at-thumbnail-p)
            (goto-char (previous-single-property-change (point) 'dired-video-thumbnail-file (point-min))))))
       (next (goto-char next))
       (prev
        (goto-char prev)
        (unless (dired-video-thumbnail--at-thumbnail-p)
          (let ((start (previous-single-property-change (point) 'dired-video-thumbnail-file)))
            (when start (goto-char start)))))))))

;;; Major mode

(define-derived-mode dired-video-thumbnail-mode special-mode "Video-Thumbs"
  "Major mode for viewing video thumbnails.

\\{dired-video-thumbnail-mode-map}"
  :group 'dired-video-thumbnail
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq header-line-format '(:eval (dired-video-thumbnail--header-line)))
  (add-hook 'post-command-hook #'dired-video-thumbnail--snap-to-thumbnail nil t))

;;; Cleanup

(defun dired-video-thumbnail--cleanup ()
  "Clean up any running processes."
  (dolist (proc dired-video-thumbnail--processes)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq dired-video-thumbnail--processes nil)
  (setq dired-video-thumbnail--pending nil))

;; Load transient menu support if available
(when (require 'dired-video-thumbnail-transient nil t)
  (dired-video-thumbnail-transient-setup-keys))

(provide 'dired-video-thumbnail)
;;; dired-video-thumbnail.el ends here
