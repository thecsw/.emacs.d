;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250426.1033"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.0.2.0")
    (llama         "0.6.2")
    (magit-section "4.3.2")
    (seq           "2.24")
    (transient     "0.8.7")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "95ee9d8c5102bc5619f0e2c6d50104cae17b2009"
  :revdesc "95ee9d8c5102"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
