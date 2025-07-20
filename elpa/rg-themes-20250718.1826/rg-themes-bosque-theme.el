;;; rg-themes-bosque-theme.el --- A night walk through the forest -*- lexical-binding: t; -*-

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

;; Bosque is a dark theme featuring a green background with blue
;; accents.

;;; Code:

(require 'rg-themes)

(defconst rg-themes-bosque-palette
  (rg-themes-define-palette
    ;; The palette colours
    '((main-bg . "#0f140b")
      (main-fg . "#e9efe4")

      (bosque-oscuro-extra . "#222b1b")

      (bosque-oscuro-0 . "#37442d")
      (bosque-oscuro-1 . "#2f3a26")
      (bosque-oscuro-2 . "#293421")

      (bosque-code-1 . "#7d916e")
      (bosque-code-2 . "#5e6f52")
      (bosque-code-3 . "#94b17d")
      (bosque-code-4 . "#bad7a2")

      (drought-grass-darker+ . "#504c2f")
      (drought-grass-darker  . "#9a946a")
      (drought-grass-dark    . "#b8b386")
      (drought-grass         . "#ddd7ab")
      (drought-grass-lighter . "#f0eac3")

      (uranus . "#b3d5fa")

      (salmon-tint    . "#f0cdc3")
      (keyword-colour . "#efdf68")
      (invigorating   . "#f2eab4")
      (some-red       . "#f8744f")
      (pinkish        . "#deb3a7"))

    ;; The palette associations
    '((background . main-bg)
      (foreground . main-fg)

      (cursor . drought-grass)
      (region . bosque-oscuro-1)
      (fringe . main-bg)

      (background-accent-strong . bosque-oscuro-0)
      (background-accent-medium . bosque-oscuro-1)
      (background-accent-light  . bosque-oscuro-2)

      (mode-line-background          . bosque-oscuro-0)
      (mode-line-foreground          . drought-grass-lighter)
      (mode-line-inactive-background . bosque-oscuro-2)
      (mode-line-inactive-foreground . drought-grass-darker)

      (accent-strong . drought-grass)
      (accent-medium . drought-grass-dark)

      (grey-neutral . bosque-code-2)
      (grey-accent  . bosque-code-1)

      (line-number             . bosque-code-2)
      (current-line-number     . bosque-code-1)
      (current-line-background . bosque-oscuro-extra)

      (white   . drought-grass-lighter)
      (black   . bosque-oscuro-0)
      (red     . some-red)
      (green   . drought-grass-darker+)
      (yellow  . keyword-colour)
      (blue    . uranus)
      (magenta . pinkish)
      (cyan    . uranus)

      (success . bosque-code-4)
      (warning . some-red)

      (built-in            . bosque-code-3)
      (preprocessor        . bosque-code-3)
      (comment             . drought-grass-darker)
      (comment-delimiter   . drought-grass-darker+)
      (comment-doc         . drought-grass-dark)
      (comment-doc-markup  . drought-grass)
      (punctuation         . drought-grass-darker+)
      (type                . invigorating)
      (function-name       . drought-grass-lighter)
      (variable-name       . bosque-code-4)
      (keyword             . uranus)
      (string              . pinkish)
      (escaped-char        . drought-grass-darker)
      (negation            . some-red)
      (number              . drought-grass-lighter)
      (constant            . salmon-tint)
      (regexp              . salmon-tint)
      (stand-out           . some-red)
      (trailing-whitespace . salmon-tint)

      (minibuffer-prompt . uranus))))

(deftheme rg-themes-bosque
  "A night walk through the forest."
  :background-mode 'light
  :family 'rg)

(rg-themes-apply-palette-for 'rg-themes-bosque 'rg-themes-bosque-palette)

(provide-theme 'rg-themes-bosque)

;;; rg-themes-bosque-theme.el ends here
