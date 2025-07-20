;;; rg-themes.el --- The rg theme collection         -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ronaldo Gligan

;; Author: Ronaldo Gligan <ronaldogligan@gmail.com>
;; URL: https://github.com/raegnald/rg-themes
;; Package-Version: 20250718.1826
;; Package-Revision: 5947efd002f2
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces

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

;; rg is a collection of light and dark themes for Emacs designed for
;; readability with low eye-strain contrast ratios. Every theme is
;; defined through the `rg-themes-define-palette' function, that
;; ensures a certain degree of consistency between all the themes in
;; this package. The themes are:
;;
;;   Ellas           (light, rg-themes-ellas-theme.el)
;;   Somnus          ( dark, rg-themes-somnus-theme.el)
;;   Cappuccino noir ( dark, rg-themes-cappuccino-noir-theme.el)
;;   Purpurina       ( dark, rg-themes-cappuccino-purpurina-theme.el)
;;   Bosque          ( dark, rg-themes-bosque-theme.el)

;;; Information specific to this file:

;; This file is the entry point for the `rg-themes' package, and
;; provides functions that make theme definition easy (see
;; `rg-themes-define-palette' and `rg-themes-apply-palette-for'). It
;; also defines the `rg-themes-set' function, which loads a theme and
;; all other customisations provided by this package (like modeline
;; padding or the "spacious" frame option).

;;; Code:

(require 'cl-lib)

(defconst rg-themes-light-themes '(rg-themes-ellas))
(defconst rg-themes-dark-themes '(rg-themes-cappuccino-noir
                                  rg-themes-somnus
                                  rg-themes-purpurina
                                  rg-themes-bosque))

(defgroup rg-themes ()
  "Options that apply to all themes in the rg collection."
  :group 'faces)

(defcustom rg-themes-modeline-padding 6
  "Amount of pixels to apply as padding to the modeline."
  :group 'rg-themes
  :type 'integer)

(defcustom rg-themes-spacious-frame t
  "Whether or not to apply extra margin to the Emacs frame."
  :group 'rg-themes
  :type 'boolean)


(defun rg-themes-define-palette (palette-colours
                                 palette-associations)
  "Define a palette.
The first argument is an alist of PALETTE-COLOURS in the
format (name . value). The argument PALETTE-ASSOCIATIONS is
another alist of colour associations."
  (declare (indent defun))
  (let ((associate-palette-name-with-colour
         (lambda (palette-association)
           (cl-destructuring-bind (name . colour) palette-association
             (cons name (or (alist-get colour palette-colours)
                            colour))))))
    (mapcar associate-palette-name-with-colour
            palette-associations)))


(defun rg-themes--colour (palette &rest names)
  "Return a color from a PALETTE.
Takes a list of colour NAMES and returns the colour with the
first matching name."
  (let (colour)
    (while (and (null colour) names)
      (setq colour (alist-get (pop names) (eval palette))))
    colour))

(defun rg-themes-apply-palette-for (theme-name palette)
  "Apply a PALETTE for a theme with name THEME-NAME."
  (custom-theme-set-faces
   theme-name
   `(default ((t (:background ,(rg-themes--colour palette 'background)
                              :foreground ,(rg-themes--colour palette 'foreground)))))
   `(cursor ((t (:background ,(rg-themes--colour palette 'cursor)))))
   `(region ((t (:background ,(rg-themes--colour palette 'region)))))
   `(fringe ((t (:background ,(rg-themes--colour palette 'fringe 'background)))))
   `(vertical-border ((t (:foreground ,(rg-themes--colour palette
                                                          'window-border
                                                          'background-accent-strong)))))

   `(line-number              ((t (:foreground ,(rg-themes--colour palette
                                                                   'line-number
                                                                   'grey-neutral)))))
   `(line-number-current-line ((t (:foreground ,(rg-themes--colour palette
                                                                   'current-line-number
                                                                   'grey-accent)))))

   `(highlight           ((t (:background ,(rg-themes--colour palette
                                                              'primary-highlight
                                                              'background-accent-medium)))))
   `(secondary-selection ((t (:background ,(rg-themes--colour palette
                                                              'secondary-highlight
                                                              'background-accent-light) ))))

   `(mode-line          ((t (:foreground ,(rg-themes--colour palette
                                                             'mode-line-foreground
                                                             'foreground)
                             :background ,(rg-themes--colour palette
                                                             'mode-line-background
                                                             'background-accent-medium)
                             :box (:line-width ,rg-themes-modeline-padding
                                   :color ,(rg-themes--colour palette
                                                              'mode-line-background
                                                              'background-accent-medium)
                                   :style nil)))))
   `(mode-line-inactive ((t (:foreground ,(rg-themes--colour palette
                                                             'mode-line-inactive-foreground
                                                             'foreground)
                             :background ,(rg-themes--colour palette
                                                             'mode-line-inactive-background
                                                             'background-accent-light)
                             :box (:line-width ,rg-themes-modeline-padding
                                   :color ,(rg-themes--colour palette
                                                              'mode-line-inactive-background
                                                              'background-accent-light)
                                   :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,(rg-themes--colour palette
                                                     'buffer-name
                                                     'accent-medium)
                              :bold t))))

   `(ansi-color-black   ((t (:foreground ,(rg-themes--colour palette 'black)
                             :background ,(rg-themes--colour palette 'black)))))
   `(ansi-color-white   ((t (:foreground ,(rg-themes--colour palette 'white)
                             :background ,(rg-themes--colour palette 'white)))))
   `(ansi-color-red     ((t (:foreground ,(rg-themes--colour palette 'red)
                             :background ,(rg-themes--colour palette 'red)))))
   `(ansi-color-green   ((t (:foreground ,(rg-themes--colour palette 'green)
                             :background ,(rg-themes--colour palette 'green)))))
   `(ansi-color-yellow  ((t (:foreground ,(rg-themes--colour palette 'yellow)
                             :background ,(rg-themes--colour palette 'yellow)))))
   `(ansi-color-blue    ((t (:foreground ,(rg-themes--colour palette 'blue)
                             :background ,(rg-themes--colour palette 'blue)))))
   `(ansi-color-magenta ((t (:foreground ,(rg-themes--colour palette 'magenta)
                             :background ,(rg-themes--colour palette 'magenta)))))
   `(ansi-color-cyan    ((t (:foreground ,(rg-themes--colour palette 'cyan)
                             :background ,(rg-themes--colour palette 'cyan)))))

   `(success ((t (:foreground ,(rg-themes--colour palette 'success 'green)
                  :weight bold))))
   `(warning ((t (:foreground ,(rg-themes--colour palette 'warning 'red)
                  :weight bold))))

   `(font-lock-builtin-face           ((t (:foreground ,(rg-themes--colour palette 'built-in)
                                           :bold t))))
   `(font-lock-preprocessor-face      ((t (:foreground ,(rg-themes--colour palette 'preprocessor)
                                           :bold t))))
   `(font-lock-comment-face           ((t (:foreground ,(rg-themes--colour palette 'comment)
                                           :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,(rg-themes--colour palette 'comment-delimiter)
                                           :slant italic))))
   `(font-lock-doc-face               ((t (:foreground ,(rg-themes--colour palette 'comment-doc)))))
   `(font-lock-doc-markup-face        ((t (:foreground ,(rg-themes--colour palette 'comment-doc-markup)))))
   `(font-lock-punctuation-face       ((t (:foreground ,(rg-themes--colour palette 'punctuation)))))
   `(font-lock-type-face              ((t (:foreground ,(rg-themes--colour palette 'type)))))
   `(font-lock-function-name-face     ((t (:foreground ,(rg-themes--colour palette 'function-name)
                                           :bold t))))
   `(font-lock-variable-name-face     ((t (:foreground ,(rg-themes--colour palette 'variable-name)))))
   `(font-lock-keyword-face           ((t (:foreground ,(rg-themes--colour palette 'keyword)))))
   `(font-lock-string-face            ((t (:foreground ,(rg-themes--colour palette 'string)))))
   `(font-lock-escape-face            ((t (:foreground ,(rg-themes--colour palette 'escaped-char)))))
   `(font-lock-negation-char-face     ((t (:foreground ,(rg-themes--colour palette 'negation)))))
   `(font-lock-number-face            ((t (:foreground ,(rg-themes--colour palette 'number)))))
   `(font-lock-constant-face          ((t (:foreground ,(rg-themes--colour palette 'constant)))))
   `(font-lock-regexp-face            ((t (:foreground ,(rg-themes--colour palette 'regexp)))))
   `(font-lock-warning-face           ((t (:foreground ,(rg-themes--colour palette 'stand-out)
                                           :bold t))))

   `(minibuffer-prompt ((t (:foreground ,(rg-themes--colour palette
                                                            'minibuffer-prompt
                                                            'accent-strong)
                            :bold t))))

   `(italic ((t (:slant italic :underline nil))))

   `(match ((t nil)))
   `(show-paren-match ((t (:inverse-video t))))

   `(link ((t (:foreground ,(rg-themes--colour palette
                                               'link-foreground
                                               'grey-neutral)
               :underline t))))

   `(hl-line ((t (:background ,(rg-themes--colour palette
                                                  'current-line-background
                                                  'background-accent-light)))))

   `(trailing-whitespace ((t (:background ,(rg-themes--colour palette
                                                              'trailing-whitespace
                                                              'red)))))
   `(whitespace-trailing ((t (:background ,(rg-themes--colour palette
                                                              'trailing-whitespace
                                                              'red)))))
   `(whitespace-empty ((t (:background ,(rg-themes--colour palette
                                                           'trailing-whitespace
                                                           'red)
                           :foreground ,(rg-themes--colour palette 'foreground)))))
   `(whitespace-newline ((t (:background ,(rg-themes--colour palette
                                                             'mode-line-inactive-background
                                                             'background-accent-light)
                             :foreground ,(rg-themes--colour palette
                                                             'grey-neutral
                                                             'warm-grey
                                                             'foreground)))))
   `(whitespace-space ((t (:background ,(rg-themes--colour palette 'background-accent-light)
                           :foreground ,(rg-themes--colour palette 'background-accent-strong)))))
   `(whitespace-hspace ((t (:background ,(rg-themes--colour palette 'background-accent-medium)
                            :foreground ,(rg-themes--colour palette 'warm-grey)))))
   `(whitespace-indentation ((t (:background ,(rg-themes--colour palette 'background-accent-medium)
                                 :foreground ,(rg-themes--colour palette 'warm-grey)))))
   `(whitespace-line ((t (:background ,(rg-themes--colour palette 'background-accent-strong)
                          :foreground ,(rg-themes--colour palette 'accent-medium 'accent-strong 'foreground)))))))

(defun rg-themes ()
  "Get all available rg themes."
  (append rg-themes-light-themes rg-themes-dark-themes))


(defvar rg-themes-after-theme-load-hook nil
  "Hook that is run after a theme has loaded.")

(defun rg-themes-spacious-frame-style-tweaks ()
  "Apply the stylistic tweaks that make the frame sleek."
  (when rg-themes-spacious-frame
    (modify-all-frames-parameters
     '((internal-border-width . 15)
       (right-divider-width . 15)
       (left-fringe . 8)
       (right-fringe . 8)))

    (let ((bg (face-attribute 'default :background)))
      (set-face-attribute 'window-divider nil
                          :foreground bg)
      (set-face-attribute 'window-divider-first-pixel nil
                          :foreground bg)
      (set-face-attribute 'window-divider-last-pixel nil
                          :foreground bg))))

;;;###autoload
(defun rg-themes-set (theme &optional dont-disable-rest)
  "Set the current theme to THEME.

By default, this function disables all loaded themes before
applying the new one. The argument DONT-DISABLE-REST, when non
nil, inhibits this behaviour."
  (interactive
   (list
    (intern (completing-read "Theme: " (rg-themes)))))

  (unless dont-disable-rest
    (dolist (enabled-theme custom-enabled-themes)
      (disable-theme enabled-theme)))

  (load-theme theme t)

  (rg-themes-spacious-frame-style-tweaks)
  (run-hooks 'rg-themes-after-theme-load-hook))

;; Add rg themes to Emacs' load path
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'rg-themes)

;;; rg-themes.el ends here
