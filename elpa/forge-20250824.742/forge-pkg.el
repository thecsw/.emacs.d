;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250824.742"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (cond-let      "0.1")
    (closql        "2.2.2")
    (emacsql       "4.3.1")
    (ghub          "4.3.2")
    (llama         "1.0")
    (magit         "4.3.6")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.9.0")
    (yaml          "1.2.0"))
  :url "https://github.com/magit/forge"
  :commit "2fb110bd9a3e272056886b5538d72024c2d41282"
  :revdesc "2fb110bd9a3e"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
