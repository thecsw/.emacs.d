;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20251215.1734"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "30.1")
    (cond-let "0.2")
    (emacsql  "4.3"))
  :url "https://github.com/emacscollective/closql"
  :commit "cbead2f2958449e6a1c4e93e956ef20c56593bc0"
  :revdesc "cbead2f29584"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
