;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250604.2042"
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
  :commit "ad381dd7ce38696346739a944d0088e02b7a0076"
  :revdesc "ad381dd7ce38"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
