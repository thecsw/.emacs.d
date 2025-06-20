;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250619.1824"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (closql        "2.2.2")
    (emacsql       "4.3.1")
    (ghub          "4.3.2")
    (let-alist     "1.0.6")
    (llama         "0.6.3")
    (magit         "4.3.6")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.9.0")
    (yaml          "1.2.0"))
  :url "https://github.com/magit/forge"
  :commit "3735c5ddfab499a18b384f68c8801a40265f9bd3"
  :revdesc "3735c5ddfab4"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
