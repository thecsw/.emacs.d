;;; uniline.el --- Add▶ ■─UNICODE based diagrams─■ to▶ ■─text files─■ -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Package-Version: 20260305.1104
;; Package-Revision: 1d62787fe676
;; Package-Requires: ((emacs "29.1") (hydra "0.15.0"))
;; Keywords: convenience, text
;; URL: https://github.com/tbanel/uniline

;; Uniline is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Uniline is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;                ┏━━━━━━━┓
;;    ╭──────╮    ┃ thick ┣═◁═╗
;;    │ thin ┝◀━━━┫ box   ┃   ║
;;    │ box  │    ┗━━━━━━━┛   ║
;;    ╰───┬──╯         ╔══════╩═╗
;;        ↓            ║ double ║
;;        ╰────────────╢ box    ║
;;                     ╚════╤═══╝
;;      ▛▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▜   │
;;      ▌quadrant-blocks▐─◁─╯
;;      ▙▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▟
;;
;;╭─Pure text────────────────□
;;│ UNICODE characters are available to draw nice boxes and lines.
;;│ They come in 4 flavours: thin, thick, double, and quadrant-blocks.
;;│ Uniline makes it easy to draw and combine all 4 flavours.
;;│ Use the arrows on the keyboard to move around leaving a line behind.
;;╰──────────────────────────╮
;;╭─Minor mode───────────────╯
;;│ Uniline is a minor mode.  Enter it with:
;;│   M-x uniline-mode
;;│ Leave it with:
;;│   C-c C-c
;;╰──────────────────────────╮
;;╭─Fonts────────────────────╯
;;│ A font able to displays the needed UNICODE characters have to
;;│ be used.  It works well with the following families:
;;│ - DejaVu Sans Mono
;;│ - Unifont
;;│ - Hack
;;│ - JetBrains Mono
;;│ - Cascadia Mono
;;│ - Agave
;;│ - JuliaMono
;;│ - FreeMono
;;│ - Iosevka Comfy Fixed, Iosevka Comfy Wide Fixed
;;│ - Aporetic Sans Mono, Aporetic Serif Mono
;;│ - Source Code Pro
;;╰──────────────────────────╮
;;╭─UTF-8────────────────────╯
;;│ Also, the encoding of the file must support UNICODE.
;;│ One way to do that, is to add a line like this one
;;│ at the top of your file:
;;│   -*- coding:utf-8; -*-
;;╰──────────────────────────╮
;;╭─Hydra or Transient───────╯
;;│ Uniline comes with two flavours of user interfaces:
;;│ Hydra and Transient.
;;│ Both versions are compiled when installing the package.
;;│
;;│ Then one or the other packages must be loaded (not both)
;;│ for example with:
;;│   (require 'uniline-hydra)
;;│ or
;;│   (use-package uniline-hydra
;;│     :bind ("C-<insert>" . uniline-mode))
;;│
;;│ This file, uniline-core.el, is the largest one, the one
;;│ implementing all the core functions independent from
;;│ Hydra or Transient
;;╰──────────────────────────□

;;; Requires:
(require 'uniline-hydra)

;; this is an alias for uniline-hydra

(provide 'uniline)
;;; uniline.el ends here
