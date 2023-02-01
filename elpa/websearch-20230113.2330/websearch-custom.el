;;; websearch-custom.el --- Customization for websearch -*- lexical-binding: t -*-


;; This file is part of websearch - query search engines from Emacs.
;; Copyright (c) 2022-2023, Maciej Barć <xgqt@riseup.net>
;; Licensed under the GNU GPL v2 License
;; SPDX-License-Identifier: GPL-2.0-or-later

;; websearch is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; websearch is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with websearch.  If not, see <https://www.gnu.org/licenses/>.



;;; Commentary:


;; Customization for ‘websearch’.



;;; Code:


(defcustom websearch-custom-engines
  '(("anarchistlibrary" ?+  "theanarchistlibrary.org/search?query=" (text archive))
    ("britannica"       ?\s "www.britannica.com/search?query=" (text dictionary))
    ("c++-docs"         ?+  "cplusplus.com/search.do?q=" (text))
    ("codeberg"         ?+  "codeberg.org/explore/repos?q=" (text code-forge))
    ("dailymotion"      ?\s "www.dailymotion.com/search/" (video))
    ("die"              ?+  "www.die.net/search/?q=" (text))
    ("django-docs"      ?+  "docs.djangoproject.com/en/4.0/search/?q=" (text))
    ("duckduckgo"       ?\s "duckduckgo.com/?q=" (text generic))
    ("gentoo-bugs"      ?+  "bugs.gentoo.org/buglist.cgi?quicksearch=" (text bugs))
    ("gentoo-overlays"  ?+  "gpo.zugaina.org/Search?search=" (text))
    ("gentoo-packages"  ?+  "packages.gentoo.org/packages/search?q=" (text))
    ("gentoo-wiki"      ?+  "wiki.gentoo.org/index.php?search=" (text))
    ("github"           ?+  "github.com/search?q=" (text code-forge))
    ("gitlab"           ?+  "gitlab.com/search?search=" (text code-forge))
    ("google"           ?\s "google.com/search?q=" (text generic))
    ("google-maps"      ?+  "maps.google.com/maps?q=" (image geography))
    ("imdb"             ?+  "imdb.com/find?q=" (video movie))
    ("julia-docs"       ?+  "docs.julialang.org/en/v1/search/?q=" (text julia))
    ("julia-packages"   ?+  "juliapackages.com/packages?search=" (text julia))
    ("melpa"            ?+  "melpa.org/#/?q=" (text melpa))
    ("melpa-stable"     ?+  "stable.melpa.org/#/?q=" (text melpa))
    ("movie-archive"    ?+  "archive.org/details/movies?query=" (video archive movie))
    ("odysee"           ?+  "odysee.com/$/search?q=" (video))
    ("peertube"         ?+  "search.joinpeertube.org/search?search=" (video))
    ("python-docs"      ?+  "docs.python.org/3/search.html?q=" (text python))
    ("python-packages"  ?+  "pypi.org/search/?q=" (text python))
    ("qwant"            ?\s "qwant.com/?q=" (text generic))
    ("racket-docs"      ?\s "docs.racket-lang.org/search/index.html?q=" (text racket))
    ("racket-packages"  ?+  "pkgd.racket-lang.org/pkgn/search?q=" (text racket))
    ("reddit"           ?+  "reddit.com/search/?q=" (text social-media))
    ("repology"         ?-  "repology.org/projects/?search=" (text))
    ("rust-packages"    ?+  "crates.io/search?q=" (text rust))
    ("sjp"              ?\s "sjp.pwn.pl/slowniki/" (text dictionary))
    ("softwareheritage" ?+  "archive.softwareheritage.org/browse/search/?q=" (text archive code-forge))
    ("stackoverflow"    ?+  "stackoverflow.com/search?q=" (text bugs))
    ("tiktok"           ?\s "www.tiktok.com/search?q=" (video social-media))
    ("twitter"          ?+  "twitter.com/search?q=" (text social-media))
    ("unicode-table"    ?+  "unicode-table.com/en/search/?q=" (text))
    ("urbandictionary"  ?+  "urbandictionary.com/define.php?term=" (text dictionary))
    ("wikipedia-en"     ?_  "en.wikipedia.org/wiki/" (text dictionary))
    ("wikipedia-pl"     ?_  "pl.wikipedia.org/wiki/" (text dictionary))
    ("wolframalpha"     ?+  "wolframalpha.com/input/?i=" (text))
    ("yandex"           ?\s "yandex.com/search/?text=" (text generic))
    ("yewtube"          ?+  "yewtu.be/search?q=" (video))
    ("youtube"          ?+  "youtube.com/results?search_query=" (video)))

  "List of supported search engines.

Each element in this list is a list of four elements:
- name of the search engine, string,
  for example: \"duckduckgo\" or \"wikipedia-en\",
- separator used for queries, character,
  for example: ?+, ?_ or ?\s (space character),
  may also be a string \"+\", \"_\" or \" \" (space character),
- query URL (without \"https://\" prefix), string,
  for example: \"duckduckgo.com/?q=\" or \"en.wikipedia.org/wiki/\"
- tags, list of symbols,
  tags should be as genral as possible but at the same time as \"specialized\"
  as possible.
  Because they are used to search all sources at once it would make little
  sense to, for example, search both python & racket package repositories
  when using a \"packages\" tag.
  For example: (dictionary), (video).
  Actually '(dictionary), but the search engines list is already quoted."
  :type '(repeat (list string character string (repeat symbol)))
  :group 'websearch)

(defcustom websearch-custom-groups
  '("codeberg, github, gitlab, repology, softwareheritage"
    "google, duckduckgo, yandex"
    "melpa, melpa-stable, repology")
  "List of search engines to search at once.

The engines are split by the \",\" (comma) separator.

For example: \"google, duckduckgo, yandex\" means:
search google, duckduckgo and yandex a the same time.

This variable is used in the `websearch--select-engines'."
  :type '(repeat string)
  :group 'websearch)

(defcustom websearch-custom-default-engine nil
  "Default search engine.

This is the default input for `completing-read' when prompted
to select an engine.
Can be set to any name of a search engine from ‘websearch-custom-engines’."
  :type 'string
  :group 'websearch)

(defcustom websearch-custom-browse-url-function 'browse-url-default-browser
  "Function to browse a full query URL with."
  :type 'symbol
  :group 'websearch)

(defcustom websearch-custom-browse-url-function-candidates
  '(eww
    browse-url-default-browser
    browse-url-generic
    browse-url-firefox
    browse-url-chromium
    browse-url-chrome)
  "Candidates to browse a full query URL with."
  :type '(repeat symbol)
  :group 'websearch)

(defcustom websearch-custom-keymap-prefix "C-c C-s"
  "Keymap prefix for ‘websearch-mode’."
  :type 'string
  :group 'websearch)


(defun websearch--engine-names ()
  "Return the names of ‘websearch-engines’."
  (mapcar #'car websearch-custom-engines))

(defun websearch--tag-matches (tag)
  "Return search engines with a matching TAG."
  (mapcan (lambda (engine)
            (cond
             ((member tag (nth 3 engine))
              (list engine))
             (t
              nil)))
          websearch-custom-engines))

(defun websearch--encode-tag (tag)
  "Return a encoded TAG."
  (concat "#" (symbol-name tag)))

(defun websearch--decode-tag (encoded-tag)
  "Return a decoded ENCODED-TAG."
  (cond
   ((string-prefix-p "#" encoded-tag)
    (intern (replace-regexp-in-string "^#" "" encoded-tag)))
   (t
    nil)))

(defun websearch--all-tags ()
  "Return all tags available in ‘websearch-custom-engines’."
  (let ((tags '()))
    (mapc (lambda (engine)
            (mapc (lambda (tag)
                    (add-to-list 'tags tag))
                  (nth 3 engine)))
          websearch-custom-engines)
    tags))

(defun websearch--all-tags-encoded ()
  "Return all encoded tags available in ‘websearch-custom-engines’.

The tags are encoded with the `websearch--encode-tag' function."
  (sort (mapcar #'websearch--encode-tag (websearch--all-tags)) #'string<))


(provide 'websearch-custom)



;;; websearch-custom.el ends here
