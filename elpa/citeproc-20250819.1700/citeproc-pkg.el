;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "citeproc" "20250819.1700"
  "A CSL 1.0.2 Citation Processor."
  '((emacs             "26")
    (dash              "2.13.0")
    (s                 "1.12.0")
    (f                 "0.18.0")
    (queue             "0.2")
    (string-inflection "1.0")
    (org               "9")
    (parsebib          "2.4")
    (compat            "28.1"))
  :url "https://github.com/andras-simonyi/citeproc-el"
  :commit "f5217b9fdbcb41a0381ecf92108390fc843090dd"
  :revdesc "f5217b9fdbcb"
  :keywords '("bib")
  :authors '(("András Simonyi" . "andras.simonyi@gmail.com"))
  :maintainers '(("András Simonyi" . "andras.simonyi@gmail.com")))
