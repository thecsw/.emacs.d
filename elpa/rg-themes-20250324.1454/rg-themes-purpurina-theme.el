;;; rg-themes-purpurina-theme.el --- I'm a star                -*- lexical-binding: t; -*-

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

;; A glamorous theme for your nocturnal needs.

;;; Code:

(require 'rg-themes)

(defconst rg-themes-purpurina-palette
  (rg-themes-define-palette
    ;; Palette colours
    '((main-bg . "#21071d")
      (main-fg . "#dac0d7")

      (grey-1 . "#dbcdd9")
      (grey-2 . "#bdafbb")
      (grey-3 . "#9e919c")
      (grey-4 . "#847882")

      (makeup-powder       . "#d3899b")
      (kiss                . "#fbd8f7")
      (kiss-darkened       . "#e1b8db")
      (kiss-darkened-twice . "#cba2c6")
      (blush-essence       . "#e2adf5")
      (chicle              . "#ff9dbc")
      (chicle-darker       . "#e087a3")
      (money-rubber        . "#ff9d9e")
      (orangeish           . "#f7a991")

      (paradise-green . "#afe89b")

      (purpu-0 . "#310c2c")
      (purpu-1 . "#4d1546")
      (purpu-2 . "#6f2765")
      (purpu-3 . "#8e3882")
      (purpu-4 . "#ae54a2")

      (golden              . "#f2ee69")
      (golden-darker       . "#c6c132")

      (liquid-blue-darker  . "#3d93a4")
      (liquid-blue         . "#53bacf")
      (liquid-blue-lighter . "#8bdbeb"))

    ;; Palette associations
    '((background . main-bg)
      (foreground . main-fg)

      (cursor . golden)
      (region . purpu-1)

      (background-accent-medium . purpu-1)
      (background-accent-light  . purpu-0)

      (accent-strong . golden)
      (accent-medium . liquid-blue-darker)

      (minibuffer-prompt . golden-darker)

      (buffer-name . kiss)

      (line-number             . purpu-2)
      (current-line-number     . purpu-3)
      (current-line-background . purpu-0)

      (white   . kiss)
      (black   . purpu-2)
      (red     . money-rubber)
      (green   . paradise-green)
      (yellow  . golden)
      (blue    . liquid-blue)
      (magenta . purpu-4)
      (cyan    . liquid-blue-lighter)

      (success . paradise-green)
      (warning . money-rubber)

      (built-in            . kiss-darkened-twice)
      (preprocessor        . kiss-darkened-twice)
      (comment             . grey-4)
      (comment-delimiter   . grey-4)
      ;; (comment-doc         . )
      ;; (comment-doc-markup  . )
      ;; (punctuation         . )
      (type                . blush-essence)
      (function-name       . kiss)
      (variable-name       . kiss-darkened)
      (keyword             . chicle-darker)
      (string              . makeup-powder)
      (escaped-char        . orangeish)
      (negation            . money-rubber)
      (number              . orangeish)
      (constant            . orangeish)
      (regexp              . orangeish)
      (stand-out           . golden-darker)
      (trailing-whitespace . money-rubber))))

(deftheme rg-themes-purpurina
  "A glamorous theme for your nocturnal needs."
  :background-mode 'dark
  :family 'rg)

(rg-themes-apply-palette-for 'rg-themes-purpurina
                             'rg-themes-purpurina-palette)

(provide-theme 'rg-themes-purpurina)

;;; rg-themes-purpurina-theme.el ends here
