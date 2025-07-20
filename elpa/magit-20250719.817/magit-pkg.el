;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250719.817"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "1.0.0")
    (magit-section "4.3.8")
    (seq           "2.24")
    (transient     "0.9.3")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "a1258ac487e0e77b28660effe56a5ae92a673709"
  :revdesc "a1258ac487e0"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
