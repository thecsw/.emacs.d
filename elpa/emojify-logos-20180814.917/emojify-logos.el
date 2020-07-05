;;; emojify-logos.el --- Add logos to emojify

;; Copyright (C) 2017 mxgoldstein

;; Author: mxgoldstein <m_goldstein@gmx.net>
;; URL: https://github.com/mxgoldstein/emojify-logos
;; Version: 0.1
;; Package-Requires: ((emojify "0.4"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;;; Commentary:

;; This package adds logo icons for various programming languages and tools to emojify.el
;; All icons are the property of their respective owners and may be trademarked and/or restricted in the way they may be used.  See COPYRIGHT.MD for more details.

;;; TODO

;; Add more logos for languages / modes etc.
;; Replace low-quality C icon
;; Find a (somewhat) free icon for java

;;; Code:

(require 'emojify)

(setq emojify-logo-list '("AngularJS" "C" "CSS3" "C++" "Emacs" "Erlang" "Haskell" "HTML5" "JavaScript" "LaTeX" "Org" "PDF" "Perl" "PHP" "Python" "Ruby" "TypeScript"))

(let (logo)
  (dolist (logo emojify-logo-list)
    (add-to-list 'emojify-user-emojis
                 `(,(concat ":" (downcase logo) ":") . (( "name" . ,logo )
                                                         ("image" . ,(concat (file-name-directory (or load-file-name buffer-file-name)) "/logos/" (downcase logo) ".png"))
                                                         ("style" . "github"))))))

;; Update emoji set
(emojify-set-emoji-data)

(provide 'emojify-logos)

;;; emojify-logos.el ends here
