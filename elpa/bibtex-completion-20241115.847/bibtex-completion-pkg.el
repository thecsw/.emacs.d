;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "bibtex-completion" "20241115.847"
  "A BibTeX backend for completion frameworks."
  '((parsebib "6.0")
    (s        "1.9.0")
    (dash     "2.6.0")
    (f        "0.16.2")
    (cl-lib   "0.5")
    (biblio   "0.2")
    (emacs    "26.1"))
  :url "https://github.com/tmalsburg/helm-bibtex"
  :commit "3c75c2cad12942c45e063d051f93e61cde58a539"
  :revdesc "3c75c2cad129"
  :authors '(("Titus von der Malsburg" . "malsburg@posteo.de")
             ("Justin Burkett" . "justin@burkett.cc"))
  :maintainers '(("Titus von der Malsburg" . "malsburg@posteo.de")))
