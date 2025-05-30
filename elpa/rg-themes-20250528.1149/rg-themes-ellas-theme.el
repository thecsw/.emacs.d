;;; rg-themes-ellas-theme.el --- A trip to Greece              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ronaldo Gligan

;; Author: Ronaldo Gligan <ronaldogligan@gmail.com>
;; URL: https://github.com/raegnald/rg-themes
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

;; Ellas is light Emacs theme inspired by the colours of Greece.

;;; Code:

(require 'rg-themes)

(defconst rg-themes-ellas-palette
  (rg-themes-define-palette
    ;; The palette colours
    '((main-bg              . "#f8ede4")
      (main-fg              . "#3d3b23")

      (bleached-silk        . "#f6f6f5")

      (athens-sculpture-1   . "#d3c3b6")
      (athens-sculpture-2   . "#f1e7de")

      (warm-grey            . "#59574c")
      (street-black         . "#2a241f")

      (street-pink          . "#be8a90")
      (charming-red         . "#c24639")
      (agora-tile-red       . "#b45952")
      (tourist-shoes-brown  . "#3c2a19")
      (aged-jade            . "#6b6757")
      (dark-sunshine        . "#b09a49")
      (olive-green          . "#898442")
      (olive-green-darker   . "#665f18")
      (blueish-grey         . "#607a96")
      (blueish-grey-lighter . "#97c0eb")
      (ellas-blue           . "#2b324c")
      (dark-neon-blue       . "#001e86")
      (sky-blue-athens      . "#074a96")
      (nice-light-blue      . "#497fc1"))

    ;; The palette associations
    '((background . main-bg)
      (foreground . main-fg)

      (cursor . olive-green)
      (region . athens-sculpture-1)
      (fringe . main-bg)

      (background-accent-strong . athens-sculpture-1)
      (background-accent-medium . athens-sculpture-1)
      (background-accent-light  . athens-sculpture-2)

      (mode-line-background          . athens-sculpture-1)
      (mode-line-inactive-background . athens-sculpture-2)

      (accent-strong . sky-blue-athens)
      (accent-medium . ellas-blue)

      (grey-neutral . warm-grey)
      (grey-accent  . street-black)

      (line-number             . warm-grey)
      (current-line-number     . street-black)
      (current-line-background . athens-sculpture-2)

      (white   . bleached-silk)
      (black   . street-black)
      (red     . charming-red)
      (green   . olive-green)
      (yellow  . dark-sunshine)
      (blue    . sky-blue-athens)
      (magenta . street-pink)
      (cyan    . dark-neon-blue)

      (success . olive-green)
      (warning . agora-tile-red)

      (built-in            . tourist-shoes-brown)
      (preprocessor        . tourist-shoes-brown)
      (comment             . olive-green)
      (comment-delimiter   . aged-jade)
      (comment-doc         . agora-tile-red)
      (comment-doc-markup  . aged-jade)
      (punctuation         . aged-jade)
      (type                . olive-green-darker)
      (function-name       . sky-blue-athens)
      (variable-name       . sky-blue-athens)
      (keyword             . dark-neon-blue)
      (string              . agora-tile-red)
      (escaped-char        . olive-green)
      (negation            . agora-tile-red)
      (number              . sky-blue-athens)
      (constant            . charming-red)
      (regexp              . agora-tile-red)
      (stand-out           . charming-red)
      (trailing-whitespace . agora-tile-red)

      (minibuffer-prompt . sky-blue-athens))))

(deftheme rg-themes-ellas
  "A trip to Greece."
  :background-mode 'light
  :family 'rg)

(rg-themes-apply-palette-for 'rg-themes-ellas 'rg-themes-ellas-palette)

(provide-theme 'rg-themes-ellas)

;;; rg-themes-ellas-theme.el ends here
