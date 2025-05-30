;;; rg-themes-somnus-theme.el --- The perfect companion for Emacs at night  -*- lexical-binding: t; -*-

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

;; A theme to focus and not to strain your eyes during a late night
;; software development session.

;;; Code:

(require 'rg-themes)

(defconst rg-themes-somnus-palette
  (rg-themes-define-palette
    ;; The palette colours
    '((main-bg              . "#090e13")
      (main-fg              . "#dee8f1")

      (modeline-bg          . "#24384b")
      (modeline-bg-inactive . "#162330")
      (subtle-difference    . "#0c1319")

      (light-violet         . "#dabdfc")
      (cold-lips            . "#bdbffc")
      (liliac               . "#bfb5ff")
      (pineapple            . "#e8d283")
      (champagne            . "#e2d7ae")
      (keyword-yellow       . "#f1d384")
      (make-up              . "#e2bbae")
      (almost-grey          . "#ccc9c9")
      (cotton-candy         . "#f5c5d1")
      (autumn               . "#ffb951")
      (meh-blue             . "#aac7e0")
      (region-blue          . "#225487")
      (modern-blue          . "#b7d0ea")
      (nice-blue            . "#9dcdff")
      (link-cyan            . "#14e5e5")
      (salmonish            . "#ff8660")
      (old-green-wall       . "#aad2ae")
      (almost-black         . "#05080a"))

    ;; The palette associations
    '((background . main-bg)
      (foreground . main-fg)

      (cursor      . pineapple)
      (region      . modeline-bg)
      (fringe      . main-bg)
      (buffer-name . main-fg)

      (background-accent-strong . modeline-bg)
      (background-accent-medium . modeline-bg)
      (background-accent-light  . modeline-bg-inactive)

      (accent-strong . keyword-yellow)
      (accent-medium . pineapple)
      (accent-light  . champagne)

      (grey-neutral . almost-grey)
      (grey-accent  . meh-blue)

      (line-number             . almost-grey)
      (current-line-number     . almost-grey)
      (current-line-background . subtle-difference)

      (white   . main-fg)
      (black   . almost-black)
      (red     . salmonish)
      (green   . old-green-wall)
      (yellow  . keyword-yellow)
      (blue    . region-blue)
      (magenta . cotton-candy)
      (cyan    . nice-blue)

      (success . old-green-wall)
      (warning . salmonish)

      (built-in            . liliac)
      (preprocessor        . liliac)
      (comment             . make-up)
      (comment-delimiter   . almost-grey)
      (comment-doc         . cold-lips)
      (comment-doc-markup  . light-violet)
      (punctuation         . main-fg)
      (type                . champagne)
      (function-name       . nice-blue)
      (variable-name       . modern-blue)
      (keyword             . keyword-yellow)
      (string              . cotton-candy)
      (escaped-char        . old-green-wall)
      (negation            . autumn)
      (number              . autumn)
      (constant            . autumn)
      (regexp              . cotton-candy)
      (stand-out           . salmonish)
      (trailing-whitespace . salmonish)

      (link . link-cyan)

      (minibuffer-prompt . nice-blue))))

(deftheme rg-themes-somnus
  "Relaxing, night-centred theme for Emacs.
A theme to focus and not to strain your eyes during a late night
software development session."
  :background-mode 'dark
  :family 'rg)

(rg-themes-apply-palette-for 'rg-themes-somnus
                             'rg-themes-somnus-palette)

(provide-theme 'rg-themes-somnus)

;;; rg-themes-somnus-theme.el ends here
