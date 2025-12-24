;;; dired-video-thumbnail-transient.el --- Transient menu for dired-video-thumbnail -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Dyer

;; Author: James Dyer
;; Keywords: multimedia, files, dired
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

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

;; This file provides a transient menu interface for dired-video-thumbnail.
;; It offers a comprehensive menu activated with C-c . in the thumbnail buffer.
;;
;; To use, add to your dired-video-thumbnail.el:
;;
;;   (require 'dired-video-thumbnail-transient)
;;
;; Or load it separately after dired-video-thumbnail.

;;; Code:

(require 'transient)

;; Declare variables from dired-video-thumbnail
(defvar dired-video-thumbnail--sort-by)
(defvar dired-video-thumbnail--sort-order)
(defvar dired-video-thumbnail--recursive)
(defvar dired-video-thumbnail--all-videos)
(defvar dired-video-thumbnail--current-videos)
(defvar dired-video-thumbnail-sort-by)
(defvar dired-video-thumbnail-sort-order)
(defvar dired-video-thumbnail-wrap-display)
(defvar dired-video-thumbnail-mode-map)

;; Declare functions from dired-video-thumbnail
(declare-function dired-video-thumbnail--format-active-filters "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort-by-name "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort-by-date "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort-by-size "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort-by-duration "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort-reverse "dired-video-thumbnail")
(declare-function dired-video-thumbnail-sort "dired-video-thumbnail")
(declare-function dired-video-thumbnail-filter-by-name "dired-video-thumbnail")
(declare-function dired-video-thumbnail-filter-by-duration "dired-video-thumbnail")
(declare-function dired-video-thumbnail-filter-by-size "dired-video-thumbnail")
(declare-function dired-video-thumbnail-filter-clear "dired-video-thumbnail")
(declare-function dired-video-thumbnail-filter "dired-video-thumbnail")
(declare-function dired-video-thumbnail-mark "dired-video-thumbnail")
(declare-function dired-video-thumbnail-unmark "dired-video-thumbnail")
(declare-function dired-video-thumbnail-toggle-mark "dired-video-thumbnail")
(declare-function dired-video-thumbnail-mark-all "dired-video-thumbnail")
(declare-function dired-video-thumbnail-unmark-all "dired-video-thumbnail")
(declare-function dired-video-thumbnail-toggle-all-marks "dired-video-thumbnail")
(declare-function dired-video-thumbnail-next "dired-video-thumbnail")
(declare-function dired-video-thumbnail-previous "dired-video-thumbnail")
(declare-function dired-video-thumbnail-next-row "dired-video-thumbnail")
(declare-function dired-video-thumbnail-previous-row "dired-video-thumbnail")
(declare-function dired-video-thumbnail-goto-dired "dired-video-thumbnail")
(declare-function dired-video-thumbnail-play "dired-video-thumbnail")
(declare-function dired-video-thumbnail-increase-size "dired-video-thumbnail")
(declare-function dired-video-thumbnail-decrease-size "dired-video-thumbnail")
(declare-function dired-video-thumbnail-toggle-wrap "dired-video-thumbnail")
(declare-function dired-video-thumbnail-toggle-recursive "dired-video-thumbnail")
(declare-function dired-video-thumbnail-refresh "dired-video-thumbnail")
(declare-function dired-video-thumbnail-regenerate "dired-video-thumbnail")
(declare-function dired-video-thumbnail-regenerate-all "dired-video-thumbnail")
(declare-function dired-video-thumbnail-delete "dired-video-thumbnail")
(declare-function dired-video-thumbnail-delete-marked "dired-video-thumbnail")
(declare-function dired-video-thumbnail-clear-cache "dired-video-thumbnail")
(declare-function dired-video-thumbnail-help "dired-video-thumbnail")
(declare-function dired-video-thumbnail-quit-and-kill "dired-video-thumbnail")

;;; State description function

(defun dired-video-thumbnail-transient--state-description ()
  "Return a string describing the current state."
  (if (not (boundp 'dired-video-thumbnail--sort-by))
      "Video Thumbnail Commands"
    (let* ((sort-by (or dired-video-thumbnail--sort-by
                        dired-video-thumbnail-sort-by))
           (sort-order (or dired-video-thumbnail--sort-order
                           dired-video-thumbnail-sort-order))
           (recursive dired-video-thumbnail--recursive)
           (wrap dired-video-thumbnail-wrap-display)
           (total (length dired-video-thumbnail--all-videos))
           (filtered (length dired-video-thumbnail--current-videos))
           (filters (dired-video-thumbnail--format-active-filters)))
      (concat
       (propertize "State: " 'face 'transient-heading)
       (format "Sort: %s %s | "
               (propertize (symbol-name sort-by) 'face 'transient-value)
               (if (eq sort-order 'ascending) "↑" "↓"))
       (format "Videos: %s%s | "
               (propertize (number-to-string filtered) 'face 'transient-value)
               (if (= total filtered) "" (format "/%d" total)))
       (format "Recursive: %s | "
               (propertize (if recursive "ON" "OFF")
                           'face (if recursive 'success 'shadow)))
       (format "Wrap: %s"
               (propertize (if wrap "ON" "OFF")
                           'face (if wrap 'success 'shadow)))
       (if (string-empty-p filters)
           ""
         (concat "\n" (propertize "Filters: " 'face 'transient-heading)
                 (propertize filters 'face 'transient-value)))))))

;;; Sort submenu

(transient-define-prefix dired-video-thumbnail-transient-sort ()
  "Sorting commands for video thumbnails."
  ["Sort By"
   ("n" "Name" dired-video-thumbnail-sort-by-name :transient nil)
   ("d" "Date modified" dired-video-thumbnail-sort-by-date :transient nil)
   ("s" "Size" dired-video-thumbnail-sort-by-size :transient nil)
   ("D" "Duration" dired-video-thumbnail-sort-by-duration :transient nil)]
  ["Order"
   ("r" "Reverse order" dired-video-thumbnail-sort-reverse :transient nil)
   ("S" "Interactive select" dired-video-thumbnail-sort :transient nil)])

;;; Filter submenu

(transient-define-prefix dired-video-thumbnail-transient-filter ()
  "Filtering commands for video thumbnails."
  ["Filter By"
   ("n" "Name (regexp)" dired-video-thumbnail-filter-by-name :transient nil)
   ("d" "Duration range" dired-video-thumbnail-filter-by-duration :transient nil)
   ("s" "Size range" dired-video-thumbnail-filter-by-size :transient nil)]
  ["Actions"
   ("c" "Clear all filters" dired-video-thumbnail-filter-clear :transient nil)
   ("/" "Interactive select" dired-video-thumbnail-filter :transient nil)])

;;; Mark submenu

(transient-define-prefix dired-video-thumbnail-transient-mark ()
  "Marking commands for video thumbnails."
  ["Mark"
   ("m" "Mark current" dired-video-thumbnail-mark :transient nil)
   ("u" "Unmark current" dired-video-thumbnail-unmark :transient nil)
   ("t" "Toggle current" dired-video-thumbnail-toggle-mark :transient nil)]
  ["Bulk"
   ("M" "Mark all" dired-video-thumbnail-mark-all :transient nil)
   ("U" "Unmark all" dired-video-thumbnail-unmark-all :transient nil)
   ("T" "Toggle all" dired-video-thumbnail-toggle-all-marks :transient nil)])

;;; Display submenu

(transient-define-prefix dired-video-thumbnail-transient-display ()
  "Display commands for video thumbnails."
  ["Size"
   ("+" "Increase size" dired-video-thumbnail-increase-size :transient t)
   ("-" "Decrease size" dired-video-thumbnail-decrease-size :transient t)]
  ["Toggle"
   ("w" "Wrap mode" dired-video-thumbnail-toggle-wrap :transient nil)
   ("R" "Recursive" dired-video-thumbnail-toggle-recursive :transient nil)]
  ["Refresh"
   ("r" "Refresh display" dired-video-thumbnail-refresh :transient nil)
   ("g" "Regenerate current" dired-video-thumbnail-regenerate :transient nil)
   ("G" "Regenerate all" dired-video-thumbnail-regenerate-all :transient nil)])

;;; Main transient menu

;;;###autoload
(transient-define-prefix dired-video-thumbnail-transient ()
  "Transient menu for dired-video-thumbnail."
  [:description dired-video-thumbnail-transient--state-description]
  [["Navigation"
    ("n" "Next" dired-video-thumbnail-next :transient t)
    ("p" "Previous" dired-video-thumbnail-previous :transient t)
    ("C-n" "Next row" dired-video-thumbnail-next-row :transient t)
    ("C-p" "Previous row" dired-video-thumbnail-previous-row :transient t)
    ("d" "Go to dired" dired-video-thumbnail-goto-dired :transient nil)]
   ["Playback"
    ("RET" "Play video" dired-video-thumbnail-play :transient nil)
    ("o" "Play video" dired-video-thumbnail-play :transient nil)]
   ["Sorting"
    ("s" "Sort menu..." dired-video-thumbnail-transient-sort :transient nil)
    ("S" "Interactive sort" dired-video-thumbnail-sort :transient nil)
    ("r" "Reverse order" dired-video-thumbnail-sort-reverse :transient nil)]
   ["Filtering"
    ("/" "Filter menu..." dired-video-thumbnail-transient-filter :transient nil)
    ("\\" "Interactive filter" dired-video-thumbnail-filter :transient nil)
    ("c" "Clear filters" dired-video-thumbnail-filter-clear :transient nil)]]
  [["Marking"
    ("m" "Mark menu..." dired-video-thumbnail-transient-mark :transient nil)
    ("M" "Mark all" dired-video-thumbnail-mark-all :transient nil)
    ("U" "Unmark all" dired-video-thumbnail-unmark-all :transient nil)
    ("t" "Toggle all marks" dired-video-thumbnail-toggle-all-marks :transient nil)]
   ["Delete"
    ("D" "Delete current" dired-video-thumbnail-delete :transient nil)
    ("x" "Delete marked" dired-video-thumbnail-delete-marked :transient nil)]
   ["Display"
    ("v" "Display menu..." dired-video-thumbnail-transient-display :transient nil)
    ("+" "Larger thumbnails" dired-video-thumbnail-increase-size :transient t)
    ("-" "Smaller thumbnails" dired-video-thumbnail-decrease-size :transient t)
    ("w" "Toggle wrap" dired-video-thumbnail-toggle-wrap :transient nil)
    ("R" "Toggle recursive" dired-video-thumbnail-toggle-recursive :transient nil)]
   ["Other"
    ("g" "Regenerate thumbnail" dired-video-thumbnail-regenerate :transient nil)
    ("G" "Regenerate all" dired-video-thumbnail-regenerate-all :transient nil)
    ("C" "Clear cache" dired-video-thumbnail-clear-cache :transient nil)
    ("?" "Help" dired-video-thumbnail-help :transient nil)
    ("q" "Quit menu" transient-quit-one)
    ("Q" "Quit buffer" dired-video-thumbnail-quit-and-kill :transient nil)]])

;;;###autoload
(defun dired-video-thumbnail-transient-setup-keys ()
  "Set up keybindings for the transient menu."
  (define-key dired-video-thumbnail-mode-map (kbd "C-c .") #'dired-video-thumbnail-transient)
  (define-key dired-video-thumbnail-mode-map (kbd ".") #'dired-video-thumbnail-transient))

(provide 'dired-video-thumbnail-transient)
;;; dired-video-thumbnail-transient.el ends here
