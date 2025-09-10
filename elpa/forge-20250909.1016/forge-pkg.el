;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250909.1016"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (closql        "2.3")
    (cond-let      "0.1")
    (emacsql       "4.3")
    (ghub          "5.0")
    (llama         "1.0")
    (magit         "4.4")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.10")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "bbecd8947a190894570b8344490a50ebf0efd394"
  :revdesc "bbecd8947a19"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
