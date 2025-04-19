;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250419.829"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.2")
    (magit-section "4.3.2")
    (seq           "2.24")
    (transient     "0.8.7")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "d3f275373322e8c97d9cf9adcbae76b022e5a83e"
  :revdesc "d3f275373322"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
