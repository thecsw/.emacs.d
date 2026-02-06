;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20260130.1613"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (closql        "2.4")
    (cond-let      "0.2")
    (emacsql       "4.3")
    (ghub          "5.0")
    (llama         "1.0")
    (magit         "4.5")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.12")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "6c7ed3f87730b7085293db0e277c0c90a29bab38"
  :revdesc "6c7ed3f87730"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
