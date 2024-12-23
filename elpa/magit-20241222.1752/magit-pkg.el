;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20241222.1752"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.3")
    (seq           "2.24")
    (transient     "20241217")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "f4c9753d81b030f520061ebde4279dc9f433a75a"
  :revdesc "f4c9753d81b0"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
